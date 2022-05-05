module Shakefiles.Dependencies (rules) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Shakefiles.Extra (addMacOSGhcOptionFFI)


localBinDir :: String
localBinDir = "bin"


cabalInstallExe :: String -> Action ()
cabalInstallExe package =
    cmd_ "cabal" $
        addMacOSGhcOptionFFI
        [ "v2-install"
        , package
        , "--installdir", localBinDir
        -- these are currently needed because Windows doesn't support the default symlink method
        , "--install-method=copy"
        , "--overwrite-policy=always"
        ]


rules :: Rules FilePath
rules = do
    let shellcheck = localBinDir </> "shellcheck" <.> exe

    phony "dependencies" $ need
        [ "_build/cabal-dependencies.ok"
        , "_build/cabal-test-dependencies.ok"
        , shellcheck
        ]
    phony "dist-dependencies" $ need
        [ "_build/cabal-dependencies.ok"
        ]

    "_build/cabal-dependencies.ok" %> \out -> do
        need
            [ "elm-format.cabal"
            , "cabal.project"
            , "cabal.project.freeze"
            ]
        cmd_ "cabal" $ addMacOSGhcOptionFFI [ "v2-build", "--only-dependencies" ]
        writeFile' out ""

    "_build/cabal-test-dependencies.ok" %> \out -> do
        need
            [ "elm-format.cabal"
            , "cabal.project"
            , "cabal.project.freeze"
            ]
        cmd_ "cabal" $ addMacOSGhcOptionFFI [ "v2-build", "--only-dependencies", "--enable-tests" ]
        writeFile' out ""

    shellcheck %> \out -> do
        cabalInstallExe "ShellCheck"

    return shellcheck
