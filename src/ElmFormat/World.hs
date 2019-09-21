module ElmFormat.World where

import Data.Text (Text)
import System.Console.ANSI (SGR, hSetSGR)
import System.IO (hFlush, hPutStr, hPutStrLn, stdout, stderr)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text
import qualified System.Directory as Dir
import qualified System.Environment
import qualified System.Exit


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
    readUtf8File path = Text.decodeUtf8 <$> ByteString.readFile path
    writeUtf8File path content = ByteString.writeFile path $ Text.encodeUtf8 content

    doesFileExist = Dir.doesFileExist
    doesDirectoryExist = Dir.doesDirectoryExist
    listDirectory = Dir.listDirectory

    getProgName = System.Environment.getProgName

    getStdin = Text.decodeUtf8 <$> Lazy.toStrict <$> Lazy.getContents
    getLine = Prelude.getLine
    putStr = Prelude.putStr
    putStrLn = Prelude.putStrLn
    writeStdout content = Char8.putStr $ Text.encodeUtf8 content
    flushStdout = hFlush stdout
    putStrStderr = hPutStr stderr
    putStrLnStderr = hPutStrLn stderr
    putSgrStderr = hSetSGR stderr

    exitFailure = System.Exit.exitFailure
    exitSuccess = System.Exit.exitSuccess
