module ElmFormat.World where

import Prelude ()
import Relude

import Data.Text (Text)
import System.Console.ANSI (SGR, hSetSGR)
import System.IO (hFlush, hPutStr, hPutStrLn)
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory as Dir
import qualified System.Environment
import qualified System.Exit
import qualified System.IO


class Monad m => World m where
    readUtf8File :: FilePath -> m Text
    writeUtf8File :: FilePath -> Text -> m ()

    doesFileExist :: FilePath -> m Bool
    doesDirectoryExist :: FilePath -> m Bool
    listDirectory :: FilePath -> m [FilePath]

    getProgName :: m String

    getStdin :: m Text
    getLine :: m String
    putStr :: String -> m ()
    putStrLn :: String -> m ()
    writeStdout :: Text -> m ()
    flushStdout :: m ()
    putStrStderr :: String -> m ()
    putStrLnStderr :: String -> m()
    putSgrStderr :: [SGR] -> m ()

    exitFailure :: m ()
    exitSuccess :: m ()


instance World IO where
    readUtf8File path = decodeUtf8 <$> readFileBS path
    writeUtf8File path content = writeFileBS path $ encodeUtf8 content

    doesFileExist = Dir.doesFileExist
    doesDirectoryExist = Dir.doesDirectoryExist
    listDirectory = Dir.listDirectory

    getProgName = System.Environment.getProgName

    getStdin = decodeUtf8 <$> toStrict <$> Lazy.getContents
    getLine = System.IO.getLine
    putStr = System.IO.putStr
    putStrLn = System.IO.putStrLn
    writeStdout content = putBS $ encodeUtf8 content
    flushStdout = hFlush stdout
    putStrStderr = hPutStr stderr
    putStrLnStderr = hPutStrLn stderr
    putSgrStderr = hSetSGR stderr

    exitFailure = System.Exit.exitFailure
    exitSuccess = System.Exit.exitSuccess
