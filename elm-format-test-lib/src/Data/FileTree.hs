module Data.FileTree
    ( FileTree, read, write
    , doesFileExist, doesDirectoryExist
    , listDirectory
    ) where

import Prelude hiding (read)

import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.FilePath (pathSeparator, normalise)


data FileTree' f a
    = File a
    | Directory (Map f (FileTree' f a))
    deriving (Show)

instance Ord f => Semigroup (FileTree' f a) where
    (Directory a) <> (Directory b) = Directory (Map.unionWith (<>) a b)
    (File _) <> (File b) = File b
    a <> _ = a

instance Ord f => Monoid (FileTree' f a) where
    mempty = Directory mempty


singleton :: [f] -> a -> FileTree' f a
singleton [] a = File a
singleton (next : rest) a = Directory $ Map.singleton next $ singleton rest a


isFile :: FileTree' f a -> Bool
isFile (File _) = True
isFile (Directory _) = False


isDirectory :: FileTree' f a -> Bool
isDirectory (File _) = False
isDirectory (Directory _) = True


atPath :: Ord f => b -> (FileTree' f a -> b) -> [f] -> FileTree' f a -> b
atPath _ f [] tree = f tree
atPath b _ (_ : _) (File _) = b -- path descends through a file
atPath b f (next : rest) (Directory children) =
    maybe b (atPath b f rest) (Map.lookup next children)


doesFileExist' :: Ord f => [f] -> FileTree' f a -> Bool
doesFileExist' = atPath False isFile


doesDirectoryExist' :: Ord f => [f] -> FileTree' f a -> Bool
doesDirectoryExist' = atPath False isDirectory


read' :: Ord f => [f] -> FileTree' f a -> Maybe a
read' = atPath Nothing $ \case
    File a -> Just a
    Directory _ -> Nothing


listDirectory' :: Ord f => [f] -> FileTree' f a -> [f]
listDirectory' = atPath [] $ \case
    File _ -> [] -- not a directory
    Directory children -> Map.keys children


write' :: Ord f => [f] -> a -> FileTree' f a -> FileTree' f a
write' path a tree = tree <> singleton path a


type FileTree = FileTree' String

toPath :: FilePath -> [String]
toPath path = splitOn [pathSeparator] $ normalise path

doesFileExist :: FilePath -> FileTree a -> Bool
doesFileExist path tree = doesFileExist' (toPath path) tree

doesDirectoryExist :: FilePath -> FileTree a -> Bool
doesDirectoryExist path tree = doesDirectoryExist' (toPath path) tree

listDirectory :: FilePath -> FileTree a -> [FilePath]
listDirectory path tree = listDirectory' (toPath path) tree

read :: FilePath -> FileTree a -> Maybe a
read path tree = read' (toPath path) tree

write :: FilePath -> a -> FileTree a -> FileTree a
write path contents tree = write' (toPath path) contents tree
