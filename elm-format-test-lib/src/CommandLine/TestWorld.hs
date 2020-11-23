{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module CommandLine.TestWorld where

import Prelude hiding (putStr, putStrLn, readFile, writeFile)
import CommandLine.World
import Elm.Utils ((|>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, testCase)
import Test.Tasty.Golden (goldenVsStringDiff)

import qualified Control.Monad.State.Lazy as State
import Data.FileTree (FileTree)
import qualified Data.FileTree as FileTree
import qualified Data.Map.Strict as Dict
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import Data.Text (Text)
import qualified Data.Text as Text
import qualified TestWorld.Stdio as Stdio


data TestWorldState =
    TestWorldState
        { filesystem :: FileTree Text
        , stdio :: Stdio.State
        , programs :: Dict.Map String ([String] -> State.State TestWorld ())
        , lastExitCode :: Maybe Int
        }

data Lens outer inner =
    Lens (outer -> inner) (inner -> outer -> outer)

lstdio :: Lens TestWorldState Stdio.State
lstdio = Lens stdio (\x s -> s { stdio = x })

lfilesystem :: Lens TestWorldState (FileTree Text)
lfilesystem = Lens filesystem (\x s -> s { filesystem = x })

over :: Lens outer inner -> (inner -> inner) -> outer -> outer
over (Lens get set) f outer = set (f $ get outer) outer


over' :: Lens outer inner -> (inner -> (a, inner)) -> outer -> (a, outer)
over' (Lens get set) f outer = fmap (flip set outer) (f $ get outer)

from :: Lens outer b -> (b -> z) -> outer -> z
from (Lens get _) f = f . get

type TestWorld = TestWorldState


class Has t m where
    state :: (t -> (a, t)) -> m a
    modify :: (t -> t) -> m ()
    gets :: (t -> a) -> m a

instance Monad m => Has (FileTree Text) (State.StateT TestWorld m) where
    state = State.state . over' lfilesystem
    modify = State.modify . over lfilesystem
    gets = State.gets . from lfilesystem

instance Monad m => Has Stdio.State (State.StateT TestWorldState m) where
    state = State.state . over' lstdio
    modify = State.modify . over lstdio
    gets = State.gets . from lstdio


instance Monad m => World (State.StateT TestWorldState m) where
    doesFileExist = gets . (FileTree.doesFileExist :: FilePath -> FileTree Text -> Bool)
    doesDirectoryExist = gets . (FileTree.doesDirectoryExist :: FilePath -> FileTree Text -> Bool)
    listDirectory = gets . (FileTree.listDirectory :: FilePath -> FileTree Text -> [FilePath])

    readUtf8File path =
        gets $ orError . FileTree.read path
        where
            orError (Just a) = a
            orError Nothing = error $ path ++ ": does not exist"

    writeUtf8File = modify <<< FileTree.write

    getStdin = state Stdio.getStdin
    putStr = modify . Stdio.putStr
    putStrLn = modify . Stdio.putStrLn

    writeStdout text =
        putStr text

    putStrStderr = modify . Stdio.putStrStderr
    putStrLnStderr = modify . Stdio.putStrLnStderr

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


infixr 8 <<<
(<<<) :: (c -> z) -> (a -> b -> c) -> a -> b -> z
(<<<) f g a b = f $ g a b


testWorld :: [(String, String)] -> TestWorldState
testWorld files =
      TestWorldState
          { filesystem = foldl (\t (p, c) -> FileTree.write p c t) mempty $ fmap (fmap Text.pack) files
          , stdio = Stdio.empty
          , programs = mempty
          , lastExitCode = Nothing
          }


exec :: State.State s a -> s -> s
exec = State.execState


eval :: State.State s a -> s -> a
eval = State.evalState


queueStdin :: Text -> TestWorldState -> TestWorldState
queueStdin =
    over lstdio . Stdio.queueStdin


assertOutput :: [(String, String)] -> TestWorldState -> Assertion
assertOutput expectedFiles context =
    assertBool
        ("Expected filesystem to contain: " ++ show expectedFiles ++ "\nActual: " ++ show (filesystem context))
        (all (\(k,v) -> FileTree.read k (filesystem context) == Just (Text.pack v)) expectedFiles)


goldenStdout :: String -> FilePath -> TestWorldState -> TestTree
goldenStdout =
    goldenOutputStream (Stdio.fullStdout . stdio)


goldenStderr :: String -> FilePath -> TestWorldState -> TestTree
goldenStderr =
    goldenOutputStream (Stdio.fullStderr . stdio)


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
uploadFile =
    over lfilesystem <<< FileTree.write


downloadFile :: String -> TestWorld -> Maybe Text
downloadFile =
    from lfilesystem . FileTree.read


installProgram :: String -> ([String] -> State.State TestWorld ()) -> TestWorld -> TestWorld
installProgram name handler testWorld =
    testWorld { programs = Dict.insert name handler (programs testWorld) }


run :: String -> [String] -> TestWorld -> TestWorld
run name args testWorld =
    case Dict.lookup name (programs testWorld) of
        Nothing ->
            testWorld
                { stdio = Stdio.putStrLn (Text.pack (name ++ ": command not found")) (stdio testWorld)
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
