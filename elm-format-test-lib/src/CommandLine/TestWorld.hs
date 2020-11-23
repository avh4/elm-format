{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module CommandLine.TestWorld (TestWorldState, TestWorld,expectExit,expectFileContents,goldenExitStdout,run,installProgram,init,uploadFile,goldenStderr,eval,queueStdin, goldenStdout) where

import Prelude hiding (putStr, putStrLn, readFile, writeFile, init)
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
import Control.Monad.Identity (Identity)


data TestWorldState =
    TestWorldState
        { filesystem :: FileTree Text
        , stdio :: Stdio.State
        , programs :: Dict.Map String ([String] -> State.State TestWorldState ())
        , lastExitCode :: LastExitCode
        }

newtype LastExitCode = LastExitCode (Maybe Int)


type TestWorld = State.StateT TestWorldState Identity


--
-- Generic types
--

class Lens s t where
    get :: s -> t
    set :: t -> s -> s

    over :: (t -> t) -> s -> s
    over f s = set (f $ get s) s

    from :: (t -> z) -> s -> z
    from f = f . get

instance Lens t t where
    get = id
    set = const

class Has t m where
    state :: (t -> (a, t)) -> m a

    modify :: (t -> t) -> m ()
    modify f = state (\t -> ((), f t))

    gets :: (t -> a) -> m a
    gets f = state (\t -> (f t, t))

instance (Monad m, Lens s t) => Has t (State.StateT s m) where
    state f = State.state $ \outer -> fmap (flip set outer) (f $ get outer)
    gets f = State.gets (f . get)


--
-- Lens definitions
--

instance Lens TestWorldState (FileTree Text) where
    get = filesystem
    set x s = s { filesystem = x }

instance Lens TestWorldState Stdio.State where
    get = stdio
    set x s = s { stdio = x }

instance Lens TestWorldState LastExitCode where
    get = lastExitCode
    set x s = s { lastExitCode = x }


--
-- World instance
--

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

    exitSuccess = modify $ const (LastExitCode $ Just 0)
    exitFailure = modify $ const (LastExitCode $ Just 1)


infixr 8 <<<
(<<<) :: (c -> z) -> (a -> b -> c) -> a -> b -> z
(<<<) f g a b = f $ g a b


testWorld :: [(String, String)] -> TestWorldState
testWorld files =
      TestWorldState
          { filesystem = foldl (\t (p, c) -> FileTree.write p c t) mempty $ fmap (fmap Text.pack) files
          , stdio = Stdio.empty
          , programs = mempty
          , lastExitCode = LastExitCode Nothing
          }


exec :: State.State s a -> s -> s
exec = State.execState


eval :: State.State s a -> s -> a
eval = State.evalState


queueStdin :: Text -> TestWorldState -> TestWorldState
queueStdin =
    over . Stdio.queueStdin


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


init :: TestWorldState
init = testWorld []


uploadFile :: FilePath -> Text -> TestWorldState -> TestWorldState
uploadFile =
    over <<< FileTree.write


downloadFile :: String -> TestWorldState -> Maybe Text
downloadFile =
    from . FileTree.read


installProgram :: String -> ([String] -> State.State TestWorldState ()) -> TestWorldState -> TestWorldState
installProgram name handler testWorld =
    testWorld { programs = Dict.insert name handler (programs testWorld) }


run :: String -> [String] -> TestWorldState -> TestWorldState
run name args testWorld =
    case Dict.lookup name (programs testWorld) of
        Nothing ->
            testWorld
                { stdio = Stdio.putStrLn (Text.pack (name ++ ": command not found")) (stdio testWorld)
                , lastExitCode = LastExitCode (Just 127)
                }

        Just handler ->
            let
                ((), newTestWorld) = State.runState (handler args) testWorld
            in
                newTestWorld


expectExit :: Int -> TestWorldState -> Assertion
expectExit expectedExitCode testWorld =
    case lastExitCode testWorld of
        LastExitCode Nothing ->
            fail ("Expected last exit code to be: " ++ show expectedExitCode ++ ", but no program was executed")

        LastExitCode (Just actualExitCode) ->
            assertEqual "last exit code" expectedExitCode actualExitCode


expectFileContents :: String -> Text -> TestWorldState -> Assertion
expectFileContents filename expectedContent testWorld =
    case downloadFile filename testWorld of
        Nothing ->
            fail ("Expected file " ++ show filename ++ " to have contents, but it did not exist")

        Just actualContent ->
            assertEqual ("contents of " ++ show filename) expectedContent actualContent
