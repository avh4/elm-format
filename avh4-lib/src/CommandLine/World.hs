module CommandLine.World where

import Prelude ()
import Relude hiding (getLine, putStr)


data FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


class Monad m => World m where
    readUtf8File :: FilePath -> m Text
    readUtf8FileWithPath :: FilePath -> m (FilePath, Text)
    readUtf8FileWithPath filePath =
        (,) filePath <$> readUtf8File filePath
    writeUtf8File :: FilePath -> Text -> m ()
    writeUtf8FileNoOverwrite :: FilePath -> Text -> m ()
    writeUtf8FileNoOverwrite path content =
        do
            exists <- doesFileExist path
            case exists of
                True ->
                    error "file exists and was not marked to be overwritten"
                False ->
                    writeUtf8File path content

    doesFileExist :: FilePath -> m Bool
    doesDirectoryExist :: FilePath -> m Bool
    listDirectory :: FilePath -> m [FilePath]
    stat :: FilePath -> m FileType
    stat path =
        do
            isFile <- doesFileExist path
            isDirectory <- doesDirectoryExist path
            return $ case ( isFile, isDirectory ) of
                ( True, _ ) -> IsFile
                ( _, True ) -> IsDirectory
                ( False, False ) -> DoesNotExist

    getProgName :: m Text

    getStdin :: m Text
    getLine :: m Text
    getYesOrNo :: m Bool
    getYesOrNo =
      do  flushStdout
          input <- getLine
          case input of
            "y" -> return True
            "n" -> return False
            _   -> putStr "Must type 'y' for yes or 'n' for no: " *> getYesOrNo
    putStr :: Text -> m ()
    putStrLn :: Text -> m ()
    writeStdout :: Text -> m ()
    flushStdout :: m ()
    putStrStderr :: Text -> m ()
    putStrLnStderr :: Text -> m()

    exitFailure :: m ()
    exitSuccess :: m ()
