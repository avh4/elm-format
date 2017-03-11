module ElmFormat.Filesystem where

import System.FilePath.FilePather.Find (findp)
import System.FilePath.FilePather.FilterPredicate (filterPredicate)
import System.FilePath.FilePather.RecursePredicate (recursePredicate)
import Data.Text (pack, isSuffixOf)

hasExtension :: String -> FilePath -> fileType -> Bool
hasExtension ext path _ =
    isSuffixOf (pack ext) (pack path)


hasntFilename :: String -> FilePath -> Bool
hasntFilename name path =
      not $ (isSuffixOf (pack $ '/' : name) (pack path) || name == path)


findAllElmFiles :: FilePath -> IO [FilePath]
findAllElmFiles inputFile =
    findp
        (filterPredicate $ hasExtension ".elm")
        (recursePredicate $ hasntFilename "elm-stuff") -- TODO: not tested
        inputFile
