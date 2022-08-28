{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module CommandLine.World.IO where

import Prelude ()
import Relude hiding (getLine, putStr)

import CommandLine.World
import qualified Control.Concurrent.MVar
import qualified Control.Concurrent.PooledIO.Final as Pool
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.IO
import qualified System.IO.Unsafe

-- Stolen from: https://github.com/supermario/elm-tooling-compiler/blob/0f63c27e0f334ca680a568034953f9b3d16ba3db/ext-common/Ext/Common.hs#L82-L94
{-# NOINLINE printLock #-}
printLock :: MVar ()
printLock = System.IO.Unsafe.unsafePerformIO $ newMVar ()

{- This uses an MVar to ensure all printouts are atomic and un-garbled. -}
withPrintLock :: (t -> IO b) -> t -> IO b
withPrintLock f text = Control.Concurrent.MVar.withMVar printLock (\_ -> f text)

instance World IO where
    readUtf8File path = decodeUtf8 <$> readFileBS path
    writeUtf8File path content = writeFileBS path $ encodeUtf8 content

    doesFileExist = System.Directory.doesFileExist
    doesDirectoryExist = System.Directory.doesDirectoryExist
    listDirectory = System.Directory.listDirectory

    getProgName = fmap Text.pack System.Environment.getProgName

    getStdin = decodeUtf8 <$> toStrict <$> Lazy.getContents
    getLine = Data.Text.IO.getLine
    putStr = withPrintLock Data.Text.IO.putStr
    putStrLn = withPrintLock Data.Text.IO.putStrLn
    writeStdout content = putBS $ encodeUtf8 content
    flushStdout = System.IO.hFlush stdout
    putStrStderr = withPrintLock (Data.Text.IO.hPutStr stderr)
    putStrLnStderr = withPrintLock (Data.Text.IO.hPutStrLn stderr)

    exitFailure = System.Exit.exitFailure
    exitSuccess = System.Exit.exitSuccess

    mapMConcurrently f = Pool.run . traverse (Pool.fork . f)
