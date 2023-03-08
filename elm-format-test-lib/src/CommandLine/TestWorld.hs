{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module CommandLine.TestWorld (TestWorldState, TestWorld, lastExitCode, init,uploadFile, downloadFile, eval,queueStdin,fullStdout,golden,fullStderr,expectExit,expectFileContents,goldenExitStdout) where

import Prelude hiding (putStr, putStrLn, readFile, writeFile, init)
import CommandLine.World

import qualified Control.Monad.State.Lazy as State
import Data.FileTree (FileTree)
import qualified Data.FileTree as FileTree
import Data.Text (Text)
import qualified Data.Text as Text
import qualified TestWorld.Stdio as Stdio
import Control.Monad.Identity (Identity)
import Test.Hspec.Golden ( Golden(..) )
import qualified Data.Text.IO
import Test.Hspec.Core.Spec (Example(..), Result(..), ResultStatus(..))
import Test.Hspec (shouldBe, Expectation)
import qualified System.IO.Unsafe
import Control.Concurrent.MVar
import System.FilePath ((</>))


data TestWorldState =
    TestWorldState
        { filesystem :: FileTree Text
        , stdio :: Stdio.State
        , _lastExitCode :: LastExitCode
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

    modify' :: t -> (t -> t) -> m ()
    modify' _ = modify

    gets :: (t -> a) -> m a
    gets f = state (\t -> (f t, t))

    gets' :: t -> (t -> a) -> m a
    gets' _ = gets

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
    get = _lastExitCode
    set x s = s { _lastExitCode = x }


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

    mapMConcurrently = mapM


infixr 8 <<<
(<<<) :: (c -> z) -> (a -> b -> c) -> a -> b -> z
(<<<) f g a b = f $ g a b


testWorld :: [(String, String)] -> TestWorldState
testWorld files =
      TestWorldState
          { filesystem = foldl (\t (p, c) -> FileTree.write p c t) mempty $ fmap (fmap Text.pack) files
          , stdio = Stdio.empty
          , _lastExitCode = LastExitCode Nothing
          }


eval :: State.State s a -> s -> a
eval = State.evalState


queueStdin :: Text -> TestWorld ()
queueStdin =
    modify' (undefined :: Stdio.State) . over . Stdio.queueStdin


init :: TestWorldState
init = testWorld []


uploadFile :: FilePath -> Text -> TestWorld ()
uploadFile  =
    modify' (undefined :: FileTree Text) <<< over <<< FileTree.write


downloadFile :: FilePath -> TestWorld (Maybe Text)
downloadFile =
    gets' (undefined :: FileTree Text) . from . FileTree.read


fullStdout :: TestWorld Text
fullStdout =
    gets' (undefined :: Stdio.State) $ from Stdio.fullStdout


fullStderr :: TestWorld Text
fullStderr =
    gets' (undefined :: Stdio.State) $ from Stdio.fullStderr


lastExitCode :: TestWorld (Maybe Int)
lastExitCode =
    gets' (undefined :: LastExitCode) $ from (\(LastExitCode i) -> i)



--
-- hspec helpers
--


expectExit :: Int -> TestWorld Expectation
expectExit expected =
    fmap (\actual -> actual `shouldBe` Just expected) lastExitCode


expectFileContents :: FilePath -> Text -> TestWorld Expectation
expectFileContents filename expectedContent = do
    actual <- downloadFile filename
    return $ actual `shouldBe` Just expectedContent


goldenExitStdout :: Int -> FilePath -> TestWorld (Expectation, Golden Text)
goldenExitStdout expectedExitCode goldenFile = do
    actualExitCode <- lastExitCode
    actualStdout <- fullStdout
    return
        ( actualExitCode `shouldBe` Just expectedExitCode
        , golden goldenFile actualStdout
        )


--
-- hspec instances
--


instance Example a => Example (TestWorld a) where
    type Arg (TestWorld a) = Arg a
    evaluateExample e =
        evaluateExample (State.evalState e init)

-- Stolen from: https://github.com/supermario/elm-tooling-compiler/blob/0f63c27e0f334ca680a568034953f9b3d16ba3db/ext-common/Ext/Common.hs#L82-L94
-- This is needed by the tests using `goldenExitStdout` when using `-with-rtsopts=-N`
-- which fail with this error otherwise:
--
--     uncaught exception: IOException of type ResourceBusy
--     ./tests/usage.stdout/actual: openFile: resource busy (file is locked)
{-# NOINLINE fileLock #-}
fileLock :: MVar ()
fileLock = System.IO.Unsafe.unsafePerformIO $ newMVar ()

golden :: FilePath -> Text -> Golden Text
golden name actualOutput =
    Golden
        { output = actualOutput
        , encodePretty = Text.unpack
        , writeToFile = \a b -> withMVar fileLock (\_ -> Data.Text.IO.writeFile a b)
        , readFromFile = \a -> withMVar fileLock (\_ -> Data.Text.IO.readFile a)
        , goldenFile = "." </> name </> "golden"
        , actualFile = Just ("." </> name </> "actual")
        , failFirstTime = True
        }



--
-- A mess of instances to allow `goldenExitStdout`
-- to return both a Spec and a Golden
--


instance (Example a, Arg a ~ ()) => Example (Maybe a) where
    type Arg (Maybe a) = Arg a
    evaluateExample Nothing =
        evaluateExample (Result "" Success)
    evaluateExample (Just a) =
        evaluateExample a


instance (Example a, Example b, Arg a ~ Arg b) => Example (a, b) where
    type Arg (a, b) = Arg a
    evaluateExample (a, b) params actionWith callback = do
        ra <- evaluateExample a params actionWith callback
        rb <- evaluateExample b params actionWith callback
        return (ra <> rb)


instance Semigroup Result where
    (Result a ra) <> (Result b rb) = Result (a ++ "/" ++ b) (ra <> rb)


instance Semigroup ResultStatus where
    Success <> b = b
    Pending a1 a2 <> Success = Pending a1 a2
    Pending _ _ <> b = b
    a <> _ = a
