module Shakefiles.ListFiles where

import Development.Shake
import Shakefiles.Prelude
import Development.Shake.FilePath


read :: FilePath -> String -> Action [FilePath]
read baseDir ext =
    readFileLines ("_build/list-files" </> baseDir </> ext <.> "txt")


rules :: Rules ()
rules = do
    "_build/list-files//*.txt" %> \out -> do
        alwaysRerun
        let baseDir = nTimes 2 dropDirectory1 $ dropFileName out
        let ext = dropExtension $ takeFileName out
        files <- getDirectoryFiles ""
            [ baseDir <> "//*." <> ext ]
        writeFileChanged out (unlines files)
