{-# OPTIONS_GHC -Wno-orphans #-}
module CommandLine.TestWorld (TestWorldState, TestWorld, lastExitCode, init,uploadFile, downloadFile, eval,queueStdin,fullStdout,golden,fullStderr,expectExit,expectFileContents,goldenExitStdout, testWorld) where

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
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.Text.Lazy as Lazy
import qualified Data.ByteString.Builder as B
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LB


data TestWorldState =
    TestWorldState
        { filesystem :: FileTree ByteString
        , stdio :: Stdio.Stdio
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

instance Lens TestWorldState (FileTree ByteString) where
    get = filesystem
    set x s = s { filesystem = x }

instance Lens TestWorldState Stdio.Stdio where
    get = stdio
    set x s = s { stdio = x }

instance Lens TestWorldState LastExitCode where
    get = _lastExitCode
    set x s = s { _lastExitCode = x }


--
-- World instance
--

instance Monad m => World (State.StateT TestWorldState m) where
    doesFileExist = gets . (FileTree.doesFileExist @ByteString)
    doesDirectoryExist = gets . (FileTree.doesDirectoryExist @ByteString)
    listDirectory = gets . (FileTree.listDirectory @ByteString)

    readUtf8File path =
        gets $ orError . FileTree.read path
        where
            orError (Just a) = a
            orError Nothing = error $ path ++ ": does not exist"

    writeUtf8File = modify <<< (\path -> FileTree.write path . LB.toStrict. B.toLazyByteString)

    getStdin = Lazy.encodeUtf8 . Lazy.fromStrict <$> state Stdio.getStdin
    putStr = modify . Stdio.putStr
    putStrLn = modify . Stdio.putStrLn

    writeStdout = putStr . Lazy.toStrict . Lazy.decodeUtf8 . B.toLazyByteString

    putStrStderr = modify . Stdio.putStrStderr
    putStrLnStderr = modify . Stdio.putStrLnStderr

    getProgName =
        return "elm-format"

    exitSuccess = modify $ const (LastExitCode $ Just 0)
    exitFailure = modify $ const (LastExitCode $ Just 1)


infixr 8 <<<
(<<<) :: (c -> z) -> (a -> b -> c) -> a -> b -> z
(<<<) f g a b = f $ g a b


testWorld :: [(String, Text)] -> TestWorldState
testWorld files =
      TestWorldState
          { filesystem = foldl (\t (p, c) -> FileTree.write p c t) mempty $ fmap (fmap Text.encodeUtf8) files
          , stdio = Stdio.empty
          , _lastExitCode = LastExitCode Nothing
          }


eval :: State.State s a -> s -> a
eval = State.evalState


queueStdin :: Text -> TestWorld ()
queueStdin =
    modify' (undefined :: Stdio.Stdio) . over . Stdio.queueStdin


init :: TestWorldState
init = testWorld []


uploadFile :: FilePath -> Text -> TestWorld ()
uploadFile path text  =
    modify @(FileTree ByteString) (over $ FileTree.write path (Text.encodeUtf8 text))


downloadFile :: FilePath -> TestWorld (Maybe Text)
downloadFile =
    fmap (fmap Text.decodeUtf8) . gets @(FileTree ByteString) . from . FileTree.read


fullStdout :: TestWorld Text
fullStdout =
    gets' (undefined :: Stdio.Stdio) $ from Stdio.fullStdout


fullStderr :: TestWorld Text
fullStderr =
    gets' (undefined :: Stdio.Stdio) $ from Stdio.fullStderr


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


golden :: FilePath -> Text -> Golden Text
golden name actualOutput =
    Golden
        { output = actualOutput
        , encodePretty = Text.unpack
        , writeToFile = Data.Text.IO.writeFile
        , readFromFile = Data.Text.IO.readFile
        , testName = name
        , directory = "."
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
