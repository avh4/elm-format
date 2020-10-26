{-# LANGUAGE FlexibleInstances #-}
module CommandLine.TestWorld where

import CommandLine.World
import Elm.Utils ((|>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.Golden (goldenVsStringDiff)

import Prelude hiding (putStr, readFile, writeFile)
import qualified Control.Monad.State.Lazy as State
import Data.FileTree (FileTree)
import qualified Data.FileTree as FileTree
import qualified Data.Map.Strict as Dict
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Data.Text (Text)
import qualified Data.Text as Text


data TestWorldState =
    TestWorldState
        { filesystem :: FileTree Text
        , queuedStdin :: Text
        , stdout :: [Text]
        , stderr :: [Text]
        , programs :: Dict.Map String ([String] -> State.State TestWorld ())
        , lastExitCode :: Maybe Int
        }


type TestWorld = TestWorldState


fullStdout :: TestWorldState -> Text
fullStdout state =
    stdout state
        |> reverse
        |> mconcat


fullStderr :: TestWorldState -> Text
fullStderr state =
    stderr state
        |> reverse
        |> mconcat


instance World (State.State TestWorldState) where
    doesFileExist path =
        do
            state <- State.get
            return $ FileTree.doesFileExist path (filesystem state)

    doesDirectoryExist path =
        do
            state <- State.get
            return $ FileTree.doesDirectoryExist path (filesystem state)

    listDirectory path =
        do
            state <- State.get
            return $ FileTree.listDirectory path (filesystem state)

    readUtf8File path =
        do
            state <- State.get
            case FileTree.read path (filesystem state) of
                Nothing ->
                    error $ path ++ ": does not exist"

                Just content ->
                    return content

    writeUtf8File path content =
        do
            state <- State.get
            State.put $ state { filesystem = FileTree.write path content (filesystem state) }

    getStdin =
        do
            state <- State.get
            State.put $ state { queuedStdin = "" }
            return $ queuedStdin state

    putStr string =
        do
            state <- State.get
            State.put $ state { stdout = string : stdout state }

    putStrLn string =
        do
            state <- State.get
            State.put $ state { stdout = (string <> "\n") : stdout state }

    writeStdout text =
        putStr text

    putStrStderr string =
        do
            state <- State.get
            State.put $ state { stderr = string : stderr state }

    putStrLnStderr string =
        do
            state <- State.get
            State.put $ state { stderr = (string <> "\n") : stderr state }

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
          { filesystem = foldl (\t (p, c) -> FileTree.write p c t) mempty $ fmap (fmap Text.pack) files
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


queueStdin :: Text -> TestWorldState -> TestWorldState
queueStdin newStdin state =
    state { queuedStdin = newStdin }


assertOutput :: [(String, String)] -> TestWorldState -> Assertion
assertOutput expectedFiles context =
    assertBool
        ("Expected filesystem to contain: " ++ show expectedFiles ++ "\nActual: " ++ show (filesystem context))
        (all (\(k,v) -> FileTree.read k (filesystem context) == Just (Text.pack v)) expectedFiles)


goldenStdout :: String -> FilePath -> TestWorldState -> TestTree
goldenStdout =
    goldenOutputStream fullStdout


goldenStderr :: String -> FilePath -> TestWorldState -> TestTree
goldenStderr =
    goldenOutputStream fullStderr


goldenOutputStream :: (TestWorldState -> Text) -> String -> FilePath -> TestWorldState -> TestTree
goldenOutputStream getStream testName goldenFile state =
    goldenVsStringDiff testName
        (\ref new -> ["diff", "-u", ref, new])
        goldenFile
        (return $ LazyText.encodeUtf8 $ LazyText.fromStrict $ getStream state)


goldenExitStdout :: String -> Int -> String -> TestWorldState -> TestTree
goldenExitStdout testName expectedExitCode goldenFile state =
    testGroup testName
        [ testCase "exit code" $ state |> expectExit expectedExitCode
        , goldenStdout "stdout" goldenFile state
        ]


init :: TestWorld
init = testWorld []


uploadFile :: FilePath -> Text -> TestWorld -> TestWorld
uploadFile name content world =
    world { filesystem = FileTree.write name content (filesystem world) }


downloadFile :: String -> TestWorld -> Maybe Text
downloadFile name world =
    FileTree.read name (filesystem world)


installProgram :: String -> ([String] -> State.State TestWorld ()) -> TestWorld -> TestWorld
installProgram name handler testWorld =
    testWorld { programs = Dict.insert name handler (programs testWorld) }


run :: String -> [String] -> TestWorld -> TestWorld
run name args testWorld =
    case Dict.lookup name (programs testWorld) of
        Nothing ->
            testWorld
                { stdout = Text.pack (name ++ ": command not found") : stdout testWorld
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


expectFileContents :: String -> Text -> TestWorld -> Assertion
expectFileContents filename expectedContent testWorld =
    case downloadFile filename testWorld of
        Nothing ->
            fail ("Expected file " ++ show filename ++ " to have contents, but it did not exist")

        Just actualContent ->
            assertEqual ("contents of " ++ show filename) expectedContent actualContent
