{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CommandLine.World.RealWorld (RealWorld, runRealWorld) where

{-| This is the implementation of `World` that is used by the actual CLI.
-}

import Relude
import CommandLine.World
import qualified System.Directory
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified System.IO
import qualified System.Exit
import qualified Control.Concurrent.PooledIO.Final as Pool
import qualified System.Environment
import qualified Data.ByteString.Lazy as Lazy
import Control.Concurrent (withMVar)

newtype RealWorld a =
  RealWorld (ReaderT (MVar ()) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)


runRealWorld :: RealWorld a -> IO a
runRealWorld m = do
    printLock <- newMVar ()
    runWithPrintLock printLock m


runWithPrintLock :: MVar () -> RealWorld a -> IO a
runWithPrintLock printLock (RealWorld m) =
    runReaderT m printLock


withPrintLock :: (a -> IO b) -> a -> RealWorld b
withPrintLock f a = do
    printLock <- RealWorld ask
    liftIO $ withMVar printLock (\() -> f a)


instance World RealWorld where
    readUtf8File path = decodeUtf8 <$> readFileBS path
    writeUtf8File path content = writeFileBS path $ encodeUtf8 content

    doesFileExist = liftIO . System.Directory.doesFileExist
    doesDirectoryExist = liftIO . System.Directory.doesDirectoryExist
    listDirectory = liftIO . System.Directory.listDirectory

    getProgName = liftIO $ fmap Text.pack System.Environment.getProgName

    getStdin = liftIO $ decodeUtf8 . toStrict <$> Lazy.getContents
    getLine = liftIO Data.Text.IO.getLine
    putStr = withPrintLock Data.Text.IO.putStr
    putStrLn = withPrintLock Data.Text.IO.putStrLn
    writeStdout content = liftIO $ putBS $ encodeUtf8 content
    flushStdout = liftIO $ System.IO.hFlush stdout
    putStrStderr = withPrintLock (Data.Text.IO.hPutStr stderr)
    putStrLnStderr = withPrintLock (Data.Text.IO.hPutStrLn stderr)

    exitFailure = liftIO System.Exit.exitFailure
    exitSuccess = liftIO System.Exit.exitSuccess

    mapMConcurrently f ms = do
        printLock <- RealWorld ask
        let threads = traverse (Pool.fork . runWithPrintLock printLock . f) ms
        liftIO $ Pool.run threads
