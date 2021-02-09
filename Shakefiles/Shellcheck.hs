module Shakefiles.Shellcheck (rules) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


rules shellcheck = do
    phony "shellcheck" $ need [ "_build/shellcheck.ok" ]

    "_build/shellcheck.ok" %> \out -> do
        scriptFiles <- getDirectoryFiles ""
            [ "build.sh"
            , "tests/run-tests.sh"
            , "package/collect_files.sh"
            , "package/linux/build-in-docker.sh"
            , "package/linux/build-package.sh"
            , "package/mac/build-package.sh"
            , "package/nix/build.sh"
            , "package/nix/generate_derivation.sh"
            , "package/win/build-package.sh"
            ]
        let oks = ["_build" </> f <.> "shellcheck.ok" | f <- scriptFiles]
        need oks
        writeFile' out (unlines scriptFiles)

    "_build//*.shellcheck.ok" %> \out -> do
        let script = dropDirectory1 $ dropExtension $ dropExtension out
        need [ shellcheck, script ]
        cmd_ shellcheck "-s" "bash" script
        writeFile' out ""
