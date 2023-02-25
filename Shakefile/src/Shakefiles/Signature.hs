module Shakefiles.Signature (rules) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified System.Directory


rules = do
    homeDir <- liftIO System.Directory.getHomeDirectory
    let keybaseConfig = homeDir </> ".config" </> "keybase" </> "config.json"

    "//*.asc" %> \out -> do
        let src = dropExtension out
        need [ src ]
        cmd_ "gpg" "--detach-sign" "--armor" src
