{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}
module CommandLine.World.IO where

import Prelude ()
import Relude hiding (getLine, putStr)

import CommandLine.World
import qualified Control.Concurrent.PooledIO.Final as Pool
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.IO


instance World IO where
    readUtf8File path = decodeUtf8 <$> readFileBS path
    writeUtf8File path content = writeFileBS path $ encodeUtf8 content

    doesFileExist = System.Directory.doesFileExist
    doesDirectoryExist = System.Directory.doesDirectoryExist
    listDirectory = System.Directory.listDirectory

    getProgName = fmap Text.pack System.Environment.getProgName

    getStdin = decodeUtf8 <$> toStrict <$> Lazy.getContents
    getLine = Data.Text.IO.getLine
    putStr = Data.Text.IO.putStr
    putStrLn = Data.Text.IO.putStrLn
    writeStdout content = putBS $ encodeUtf8 content
    flushStdout = System.IO.hFlush stdout
    putStrStderr = Data.Text.IO.hPutStr stderr
    putStrLnStderr = Data.Text.IO.hPutStrLn stderr

    exitFailure = System.Exit.exitFailure
    exitSuccess = System.Exit.exitSuccess

    mapMConcurrently f = Pool.run . traverse (Pool.fork . f)
