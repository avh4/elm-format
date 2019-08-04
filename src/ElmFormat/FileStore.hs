module ElmFormat.FileStore
  ( FileStore
  , FileStoreF(..)
  , FileType(..)
  , readFile
  , stat
  , listDirectory
  , execute
  )
where

import           Prelude                           hiding ( readFile
                                                          , writeFile
                                                          )
import           Control.Monad.Free
import           Data.Text                                ( Text )
import           ElmFormat.World                   hiding ( readFile
                                                          , listDirectory
                                                          )
import qualified ElmFormat.World               as World


data FileType
    = IsFile
    | IsDirectory
    | DoesNotExist


class Functor f => FileStore f where
    readFile :: FilePath -> f Text
    stat :: FilePath -> f FileType
    listDirectory :: FilePath -> f [FilePath]


data FileStoreF a
    = ReadFile FilePath (Text -> a)
    | Stat FilePath (FileType -> a)
    | ListDirectory FilePath ([FilePath] -> a)


instance Functor FileStoreF where
  fmap f (ReadFile      path a) = ReadFile path (f . a)
  fmap f (Stat          path a) = Stat path (f . a)
  fmap f (ListDirectory path a) = ListDirectory path (f . a)


instance FileStore FileStoreF where
  readFile path = ReadFile path id
  stat path = Stat path id
  listDirectory path = ListDirectory path id


instance FileStore f => FileStore (Free f) where
  readFile path = liftF (readFile path)
  stat path = liftF (stat path)
  listDirectory path = liftF (listDirectory path)


execute :: World m => FileStoreF a -> m a
execute operation = case operation of
  ReadFile path next -> next <$> readUtf8File path

  Stat     path next -> do
    isFile      <- doesFileExist path
    isDirectory <- doesDirectoryExist path
    case (isFile, isDirectory) of
      (True , _    ) -> return $ next IsFile
      (_    , True ) -> return $ next IsDirectory
      (False, False) -> return $ next DoesNotExist

  ListDirectory path next -> next <$> World.listDirectory path
