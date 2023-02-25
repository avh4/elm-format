module Shakefiles.NestedCheckout where

import Development.Shake
import Development.Shake.FilePath
import qualified System.Directory

rules :: Rules ()
rules = do
    "_build/git/checkout/*.ok" %> \out -> do
        let cloneDir = dropExtension out
        let sha = dropDirectory1 $ dropDirectory1 $ dropDirectory1 cloneDir
        exists <- liftIO $ System.Directory.doesDirectoryExist cloneDir
        if exists
            then return ()
            else cmd_ "git" "clone" "--local" "." cloneDir
        cmd_ (Cwd cloneDir) "git" "reset" "--hard" sha
        cmd_ (Cwd cloneDir) "./dev/build.sh" "generated"
        writeFileChanged out sha
