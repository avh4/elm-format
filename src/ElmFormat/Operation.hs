module ElmFormat.Operation (Operation, OperationF(..), deprecatedIO) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Free
import ElmFormat.FileStore


class FileStore f => Operation f where
    deprecatedIO :: IO a -> f a


data OperationF a
    = InFileStore (FileStoreF a)
    | DeprecatedIO (IO a)


instance Operation OperationF where
    deprecatedIO = DeprecatedIO


instance FileStore OperationF where
    readFile path = InFileStore $ readFile path
    stat path = InFileStore $ stat path
    listDirectory path = InFileStore $ listDirectory path


instance Functor OperationF where
    fmap f (InFileStore fileStore) = InFileStore (fmap f fileStore)
    fmap f (DeprecatedIO io) = DeprecatedIO (fmap f io)


instance Operation f => Operation (Free f) where
    deprecatedIO io = liftF (deprecatedIO io)
