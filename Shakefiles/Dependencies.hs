module Shakefiles.Dependencies (rules) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


rules = do
    StdoutTrim stackLocalBin <- liftIO $ cmd "stack path --local-bin"

    let shellcheck = stackLocalBin </> "shellcheck" <.> exe

    phony "dependencies" $ need [ shellcheck ]

    shellcheck %> \out -> do
        cmd_ "stack install ShellCheck"

    return shellcheck
