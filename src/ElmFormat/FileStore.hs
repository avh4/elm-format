module ElmFormat.FileStore (FileStore, FileType(..), stat, listDirectory, execute) where

import qualified System.Directory as Dir

import Prelude hiding (readFile, writeFile)
import Control.Monad.Free
import Data.Text


data FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


class FileStore f where
    readFile :: FilePath -> f Text
    stat :: FilePath -> f FileType
    listDirectory :: FilePath -> f [FilePath]


data FileStoreF a
    = ReadFile FilePath (Text -> a)
    | Stat FilePath (FileType -> a)
    | ListDirectory FilePath ([FilePath] -> a)


instance Functor FileStoreF where
    fmap f (ReadFile path a) = ReadFile path (f . a)
    fmap f (Stat path a) = Stat path (f . a)
    fmap f (ListDirectory path a) = ListDirectory path (f . a)


instance FileStore FileStoreF where
    readFile path = ReadFile path id
    stat path = Stat path id
    listDirectory path = ListDirectory path id


instance (FileStore f, Functor f) => FileStore (Free f) where
    readFile path = liftF (readFile path)
    stat path = liftF (stat path)
    listDirectory path = liftF (listDirectory path)


execute :: FileStoreF a -> IO a
execute operation =
    case operation of
        ReadFile _ _ ->
            undefined

        Stat path next ->
            do
                isFile <- Dir.doesFileExist path
                isDirectory <- Dir.doesDirectoryExist path
                case ( isFile, isDirectory ) of
                    ( True, _ ) -> return $ next IsFile
                    ( _, True ) -> return $ next IsDirectory
                    ( False, False ) -> return $ next DoesNotExist

        ListDirectory path next ->
            next <$> Dir.listDirectory path
