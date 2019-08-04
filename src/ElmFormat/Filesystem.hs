module ElmFormat.Filesystem where

import           Control.Monad.Free
import           ElmFormat.FileStore
import           System.FilePath                          ( (</>) )
import qualified System.FilePath               as FilePath


collectFiles :: Monad m => (a -> m [a]) -> a -> m [a]
collectFiles children root = do
  xs          <- children root
  subChildren <- mapM (collectFiles children) xs
  return $ root : concat subChildren


listDir :: FileStore f => FilePath -> Free f [FilePath]
listDir path = map (path </>) <$> listDirectory path


doesDirectoryExist :: FileStore f => FilePath -> Free f Bool
doesDirectoryExist path = do
  fileType <- stat path
  case fileType of
    IsDirectory -> return True
    _           -> return False


fileList :: FileStore f => FilePath -> Free f [FilePath]
fileList =
  let children path = if isSkippable path
        then return []
        else do
          directory <- doesDirectoryExist path
          if directory then listDir path else return []
  in  collectFiles children


isSkippable :: FilePath -> Bool
isSkippable path = or
  [ hasFilename "elm-stuff"    path
  , hasFilename "node_modules" path
  , hasFilename ".git"         path
  ]

hasExtension :: String -> FilePath -> Bool
hasExtension ext path = ext == FilePath.takeExtension path


findAllElmFiles :: FileStore f => FilePath -> Free f [FilePath]
findAllElmFiles inputFile = filter (hasExtension ".elm") <$> fileList inputFile


hasFilename :: String -> FilePath -> Bool
hasFilename name path = name == FilePath.takeFileName path
