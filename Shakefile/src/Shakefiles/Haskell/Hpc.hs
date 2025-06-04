module Shakefiles.Haskell.Hpc where

import Development.Shake
import Shakefiles.Prelude
import Development.Shake.FilePath
import System.Directory (createFileLink)
import Relude (unlessM)

mergeTixFiles :: [FilePath] -> FilePath -> Action ()
mergeTixFiles tixs out = do
    need tixs
    cmd_ "hpc" "sum" ("--output=" <> out) tixs


rules :: String -> Rules ()
rules gitSha = do
    let hpcConfig =
            [ "--hpcdir=./dist-newstyle/_coverage/build/x86_64-linux/ghc-9.6.7/elm-format-0.8.7/hpc/vanilla/mix/elm-format"
            , "--hpcdir=./dist-newstyle/_coverage/build/x86_64-linux/ghc-9.6.7/avh4-lib-0.0.0.1/hpc/vanilla/mix/avh4-lib-0.0.0.1"
            , "--hpcdir=./dist-newstyle/_coverage/build/x86_64-linux/ghc-9.6.7/elm-format-lib-0.0.0.1/hpc/vanilla/mix/elm-format-lib-0.0.0.1"
            , "--hpcdir=./dist-newstyle/_coverage/build/x86_64-linux/ghc-9.6.7/elm-format-markdown-0.0.0.1/hpc/vanilla/mix/elm-format-markdown-0.0.0.1"
            , "--srcdir=."
            , "--srcdir=avh4-lib"
            , "--srcdir=elm-format-lib"
            , "--srcdir=elm-format-markdown"
            ]

    "_build/hpc/report//*.ok" %> \out -> do
        let report = nTimes 3 dropDirectory1 $ dropExtension out
        let tixFile = "_build/hpc/run" </> report <.> "tix"
        let outFile = "_coverage" </> report </> gitSha <.> "txt"
        sourceHash <- hashNeed [ tixFile ]
        cmd_ (FileStdout outFile) "hpc" "report" tixFile hpcConfig
        writeFileChanged out sourceHash

    "_build/hpc/markup//*.ok" %> \out -> do
        let report = nTimes 3 dropDirectory1 $ dropExtension out
        let tixFile = "_build/hpc/run" </> report <.> "tix"
        let outDir = "_coverage" </> report </> gitSha
        sourceHash <- hashNeed [ tixFile ]
        cmd_ "hpc" "markup" tixFile
            ("--destdir=" <> outDir)
            hpcConfig
        let outIndex = outDir </> "index.html"
        unlessM (doesFileExist outIndex) $
            liftIO $ createFileLink "hpc_index.html" outIndex
        writeFileChanged out sourceHash
