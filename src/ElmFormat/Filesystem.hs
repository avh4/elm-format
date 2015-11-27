module ElmFormat.Filesystem where

import System.FilePath.Find (find, fileName, (/=?), (==?), extension)


findAllElmFiles :: FilePath -> IO [FilePath]
findAllElmFiles inputFile =
    find
        (fileName /=? "elm-stuff") -- TODO: not tested
        (extension ==? ".elm")
        inputFile
