module CommandLine.Filesystem where

import CommandLine.World
import System.FilePath ((</>))
import qualified System.FilePath as FilePath


collectFiles :: Monad m => (a -> m [a]) -> a -> m [a]
collectFiles children root =
    do
        xs <- children root
        subChildren <- mapM (collectFiles children) xs
        return $ root : concat subChildren


listDir :: World m => FilePath -> m [FilePath]
listDir path =
    map (path </>) <$> listDirectory path


fileList :: World m => FilePath -> m [FilePath]
fileList =
  let
      children path =
          if isSkippable path then
              return []
          else
              do
                  directory <- doesDirectoryExist path
                  if directory then listDir path else return []
  in
      collectFiles children


isSkippable :: FilePath -> Bool
isSkippable path =
    or
        [ hasFilename "elm-stuff" path
        , hasFilename "node_modules" path
        , hasFilename ".git" path
        ]

hasExtension :: String -> FilePath -> Bool
hasExtension ext path =
    ext == FilePath.takeExtension path


findAllElmFiles :: World m => FilePath -> m [FilePath]
findAllElmFiles inputFile =
    filter (hasExtension ".elm") <$> fileList inputFile


hasFilename :: String -> FilePath -> Bool
hasFilename name path =
    name == FilePath.takeFileName path
