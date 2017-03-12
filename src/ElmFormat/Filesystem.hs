module ElmFormat.Filesystem where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Data.Text (pack, isSuffixOf)


collectFiles :: Monad m => (a -> m [a]) -> a -> m [a]
collectFiles children root =
    do
        xs <- children root
        subChildren <- mapM (collectFiles children) xs
        return $ root : concat subChildren


listDir :: FilePath -> IO [FilePath]
listDir path =
    fmap (map (path </>)) $ listDirectory path


fileList :: FilePath -> IO [FilePath]
fileList =
  let
      children path =
          if hasntFilename "elm-stuff" path then
              do
                  directory <- doesDirectoryExist path
                  if directory then listDir path else return []
          else
              return []
  in
      collectFiles children


hasExtension :: String -> FilePath -> Bool
hasExtension ext path =
    isSuffixOf (pack ext) (pack path)


findAllElmFiles :: FilePath -> IO [FilePath]
findAllElmFiles inputFile =
    filter (hasExtension ".elm") <$> fileList inputFile


hasntFilename :: String -> FilePath -> Bool
hasntFilename name path =
      not $ (isSuffixOf (pack $ '/' : name) (pack path) || name == path)
