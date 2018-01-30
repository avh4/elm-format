{-# LANGUAGE FlexibleInstances #-}
module ElmFormat.TestWorld where

import ElmFormat.World

import Elm.Utils ((|>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual)
import Test.Tasty.Golden (goldenVsStringDiff)

import qualified Control.Monad.State.Lazy as State
import qualified Data.Map.Strict as Dict
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text


data TestWorldState =
    TestWorldState
        { filesystem :: Dict.Map String String
        , stdout :: [String]
        , programs :: Dict.Map String ([String] -> State.State TestWorld ())
        , lastExitCode :: Maybe Int
        }


type TestWorld = TestWorldState


fullStdout :: TestWorldState -> String
fullStdout state =
    stdout state
        |> (:) "\n" -- Append a final newline so reference files can be easily edited
        |> reverse
        |> concat


instance World (State.State TestWorldState) where
    readFile path =
        do
            state <- State.get
            case Dict.lookup path (filesystem state) of
                Nothing ->
                    error $ path ++ ": does not exist"

                Just content ->
                    return content

    writeFile path content =
        do
            state <- State.get
            State.put $ state { filesystem = Dict.insert path content (filesystem state) }

    putStrLn string =
        do
            state <- State.get
            State.put $ state { stdout = string : stdout state }

    getProgName =
        return "elm-format"

    exitSuccess =
        do
            state <- State.get
            State.put $ state { lastExitCode = Just 0 }

    exitFailure =
        do
            state <- State.get
            State.put $ state { lastExitCode = Just 1 }


testWorld :: [(String, String)] -> TestWorldState
testWorld files =
      TestWorldState
          { filesystem = Dict.fromList files
          , stdout = []
          , programs = mempty
          , lastExitCode = Nothing
          }


exec :: State.State s a -> s -> s
exec = State.execState


assertOutput :: [(String, String)] -> TestWorldState -> Assertion
assertOutput expectedFiles context =
    assertBool
        ("Expected filesystem to contain: " ++ show expectedFiles ++ "\nActual: " ++ show (filesystem context))
        (all (\(k,v) -> Dict.lookup k (filesystem context) == Just v) expectedFiles)


goldenStdout :: String -> FilePath -> TestWorldState -> TestTree
goldenStdout testName goldenFile state =
    goldenVsStringDiff testName
        (\ref new -> ["diff", "-u", ref, new])
        goldenFile
        (return $ Text.encodeUtf8 $ Text.pack $ fullStdout state)


init :: TestWorld
init = testWorld []


installProgram :: String -> ([String] -> State.State TestWorld ()) -> TestWorld -> TestWorld
installProgram name handler testWorld =
    testWorld { programs = Dict.insert name handler (programs testWorld) }


run :: String -> [String] -> TestWorld -> TestWorld
run name args testWorld =
    case Dict.lookup name (programs testWorld) of
        Nothing ->
            testWorld
                { stdout = (name ++ ": command not found") : stdout testWorld
                , lastExitCode = Just 127
                }

        Just handler ->
            let
                ((), newTestWorld) = State.runState (handler args) testWorld
            in
                newTestWorld


expectExit :: Int -> TestWorld -> Assertion
expectExit expectedExitCode testWorld =
    case lastExitCode testWorld of
        Nothing ->
            fail ("Expected last exit code to be: " ++ show expectedExitCode ++ ", but no program was executed")

        Just actualExitCode ->
            assertEqual "last exit code" expectedExitCode actualExitCode
