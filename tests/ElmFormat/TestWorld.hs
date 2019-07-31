{-# LANGUAGE FlexibleInstances #-}
module ElmFormat.TestWorld where

import ElmFormat.World

import Elm.Utils ((|>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.Golden (goldenVsStringDiff)

import Prelude hiding (putStr, readFile, writeFile)
import qualified Control.Monad.State.Lazy as State
import qualified Data.Map.Strict as Dict
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text as StrictText


data TestWorldState =
    TestWorldState
        { filesystem :: Dict.Map String String
        , queuedStdin :: String
        , stdout :: [String]
        , stderr :: [String]
        , programs :: Dict.Map String ([String] -> State.State TestWorld ())
        , lastExitCode :: Maybe Int
        }


type TestWorld = TestWorldState


fullStdout :: TestWorldState -> String
fullStdout state =
    stdout state
        |> reverse
        |> concat


fullStderr :: TestWorldState -> String
fullStderr state =
    stderr state
        |> reverse
        |> concat


instance World (State.State TestWorldState) where
    doesFileExist path =
        do
            state <- State.get
            return $ Dict.member path (filesystem state)

    doesDirectoryExist _path =
        return False

    readFile path =
        do
            state <- State.get
            case Dict.lookup path (filesystem state) of
                Nothing ->
                    error $ path ++ ": does not exist"

                Just content ->
                    return content

    readUtf8File path =
        do
            content <- readFile path
            return $ StrictText.pack content

    writeFile path content =
        do
            state <- State.get
            State.put $ state { filesystem = Dict.insert path content (filesystem state) }

    writeUtf8File path content =
        writeFile path (StrictText.unpack content)

    getStdin =
        do
            state <- State.get
            State.put $ state { queuedStdin = [] }
            return $ StrictText.pack (queuedStdin state)

    putStr string =
        do
            state <- State.get
            State.put $ state { stdout = string : stdout state }

    putStrLn string =
        do
            state <- State.get
            State.put $ state { stdout = (string ++ "\n") : stdout state }

    writeStdout text =
        putStr (StrictText.unpack text)

    putStrStderr string =
        do
            state <- State.get
            State.put $ state { stderr = string : stderr state }

    putStrLnStderr string =
        do
            state <- State.get
            State.put $ state { stderr = (string ++ "\n") : stderr state }

    putSgrStderr _ =
        -- NOTE: tests currently ignore SGRs (used for displaying colors in error messages)
        return ()

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
          , queuedStdin = ""
          , stdout = []
          , stderr = []
          , programs = mempty
          , lastExitCode = Nothing
          }


exec :: State.State s a -> s -> s
exec = State.execState


eval :: State.State s a -> s -> a
eval = State.evalState


queueStdin :: String -> TestWorldState -> TestWorldState
queueStdin newStdin state =
    state { queuedStdin = newStdin }


assertOutput :: [(String, String)] -> TestWorldState -> Assertion
assertOutput expectedFiles context =
    assertBool
        ("Expected filesystem to contain: " ++ show expectedFiles ++ "\nActual: " ++ show (filesystem context))
        (all (\(k,v) -> Dict.lookup k (filesystem context) == Just v) expectedFiles)


goldenStdout :: String -> FilePath -> TestWorldState -> TestTree
goldenStdout =
    goldenOutputStream fullStdout


goldenStderr :: String -> FilePath -> TestWorldState -> TestTree
goldenStderr =
    goldenOutputStream fullStderr


goldenOutputStream :: (TestWorldState -> String) -> String -> FilePath -> TestWorldState -> TestTree
goldenOutputStream getStream testName goldenFile state =
    goldenVsStringDiff testName
        (\ref new -> ["diff", "-u", ref, new])
        goldenFile
        (return $ Text.encodeUtf8 $ Text.pack $ getStream state)


goldenExitStdout :: String -> Int -> String -> TestWorldState -> TestTree
goldenExitStdout testName expectedExitCode goldenFile state =
    testGroup testName
        [ testCase "exit code" $ state |> expectExit expectedExitCode
        , goldenStdout "stdout" goldenFile state
        ]


init :: TestWorld
init = testWorld []


uploadFile :: String -> String -> TestWorld -> TestWorld
uploadFile name content world =
    world { filesystem = Dict.insert name content (filesystem world) }


downloadFile :: String -> TestWorld -> Maybe String
downloadFile name world =
    Dict.lookup name (filesystem world)


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


expectFileContents :: String -> String -> TestWorld -> Assertion
expectFileContents filename expectedContent testWorld =
    case downloadFile filename testWorld of
        Nothing ->
            fail ("Expected file " ++ show filename ++ " to have contents, but it did not exist")

        Just actualContent ->
            assertEqual ("contents of " ++ show filename) expectedContent actualContent
