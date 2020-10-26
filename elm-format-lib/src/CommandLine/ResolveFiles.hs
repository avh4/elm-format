module CommandLine.ResolveFiles (resolveElmFiles, Error(..)) where

-- This module provides reusable functions to resolve command line arguments into a list of Elm files

import Prelude ()
import Relude

import CommandLine.InfoFormatter (ToConsole(..))
import qualified CommandLine.Filesystem as Filesystem
import CommandLine.World (World, FileType(..))
import qualified CommandLine.World as World
import Data.Either.Extra (collectErrors)
import qualified Data.Text as Text


data Error
    = FileDoesNotExist FilePath
    | NoElmFiles FilePath


instance ToConsole Error where
    toConsole = \case
        FileDoesNotExist path -> Text.pack path <> ": No such file or directory"
        NoElmFiles path -> Text.pack path <> ": Directory does not contain any *.elm files"


resolveFile :: World m => FilePath -> m (Either Error [FilePath])
resolveFile path =
    do
        fileType <- World.stat path

        case fileType of
            IsFile ->
                return $ Right [path]

            IsDirectory ->
                do
                    elmFiles <- Filesystem.findAllElmFiles path
                    case elmFiles of
                        [] -> return $ Left $ NoElmFiles path
                        _ -> return $ Right elmFiles

            DoesNotExist ->
                return $ Left $ FileDoesNotExist path


resolveElmFiles :: World m => [FilePath] -> m (Either [Error] [FilePath])
resolveElmFiles inputFiles =
    do
        result <- collectErrors <$> mapM resolveFile inputFiles
        case result of
            Left ls ->
                return $ Left ls

            Right files ->
                return $ Right $ concat files
