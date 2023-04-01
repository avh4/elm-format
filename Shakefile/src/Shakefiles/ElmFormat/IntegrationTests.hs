module Shakefiles.ElmFormat.IntegrationTests where

import Control.Monad (forM_)
import Development.Shake
import Development.Shake.FilePath
import Shakefiles.Prelude
import qualified Shakefiles.Haskell.Hpc as Hpc
import Relude (whenM)
import System.Directory (removeFile)
import qualified Shakefiles.ListFiles as ListFiles

rules :: String -> FilePath -> Rules ()
rules gitSha elmFormat = do
    phony "integration-tests" $
        need
            [ "_build/run-tests.ok"
            , "_build/integration-tests/good-json.ok"
            , "_build/integration-tests/good-json-roundtrip.ok"
            , "_build/integration-tests/good/Elm-0.19.ok"
            , "_build/integration-tests/good/Elm-0.18.ok"
            , "_build/integration-tests/good/Elm-0.17.ok"
            , "_build/integration-tests/transform/Elm-0.19.ok"
            , "_build/integration-tests/transform/Elm-0.18.ok"
            , "_build/integration-tests/bad/Elm-0.19.ok"
            , "_build/integration-tests/from-json/Elm-0.19.ok"
            ]

    "_build/run-tests.ok" %> \out -> do
        let script = "tests/run-tests.sh"
        need [ script, elmFormat ]
        testFiles <- getDirectoryFiles ""
            [ "tests/test-files/good/json//*.elm"
            , "tests/test-files/bad//*.elm"
            , "tests/test-files/directory//*.elm"
            , "tests/test-files/recursive-directory//*.elm"
            , "tests/test-files/*.json"
            ]
        hash <- hashNeed testFiles
        cmd_ ("bash" <.> exe) script elmFormat
        writeFile' out (unlines $ hash : testFiles)

    "_build/tests/test-files/prof.ok" %> \out -> do
        let oks =
              [ "_build/tests/test-files/good/Elm-0.17/prof.ok"
              , "_build/tests/test-files/good/Elm-0.18/prof.ok"
              , "_build/tests/test-files/good/Elm-0.19/prof.ok"
              ]
        need oks
        writeFile' out (unlines oks)

    "_build/hpc/run/integration-tests.tix" %> \out ->
          Hpc.mergeTixFiles
              [ "_build/hpc/run/integration-tests/good/Elm-0.19.tix"
              , "_build/hpc/run/integration-tests/good/Elm-0.18.tix"
              , "_build/hpc/run/integration-tests/good/Elm-0.17.tix"
              , "_build/hpc/run/integration-tests/transform/Elm-0.19.tix"
              , "_build/hpc/run/integration-tests/transform/Elm-0.18.tix"
              , "_build/hpc/run/integration-tests/bad/Elm-0.19.tix"
              , "_build/hpc/run/integration-tests/from-json/Elm-0.19.tix"
              ]
              out


    -- Elm files

    "_build/integration-tests/*/*.ok" %> \out -> do
        let exampleType = takeDirectory1 $ nTimes 2 dropDirectory1 out
        let runProfile = dropExtension $ takeFileName out
        let sourceExt = case exampleType of
              "from-json" -> "json"
              _ -> "elm"
        sourceFiles <- ListFiles.read ("_build/list-files/tests/test-files" </> exampleType </> runProfile) sourceExt
        let oks = case exampleType of
              "good" -> [ "_build" </> f -<.> "elm_matches" | f <- sourceFiles]
              "bad" -> [ "_build" </> f -<.> "elm_bad_matches" | f <- sourceFiles ]
              "transform" -> [ "_build" </> f -<.> "elm_transform_matches" | f <- sourceFiles, takeExtension (dropExtension f) /= ".formatted" ]
              "from-json" -> ["_build" </> f -<.> "elm_from_json_matches" | f <- sourceFiles ]
              _ -> error ("unknown example type: " <> exampleType)
        need oks
        writeFile' out (unlines sourceFiles)

    "_build/hpc/run/integration-tests/*/*.tix" %> \out -> do
        let exampleType = takeDirectory1 $ nTimes 4 dropDirectory1 out
        let runProfile = dropExtension $ takeFileName out
        let sourceExt = case exampleType of
              "from-json" -> "json"
              _ -> "elm"
        sourceFiles <- ListFiles.read ("tests/test-files" </> exampleType </> runProfile) sourceExt
        let tixs = ["_build/hpc/run/integration-tests" </> f -<.> "tix" | f <- sourceFiles]
        Hpc.mergeTixFiles tixs out

    let elmVersions = [ "0.17", "0.18", "0.19" ]

    forM_ elmVersions $ \elmVersion -> do
        ("_build/tests/test-files/good/Elm-" ++ elmVersion ++ "/prof.ok") %> \out -> do
            elmFiles <- ListFiles.read ("tests/test-files/good/Elm-" <> elmVersion) "elm"
            let oks = ["_profile" </> f -<.> (gitSha ++ ".prof") | f <- elmFiles]
            need oks
            writeFile' out (unlines oks)

        ("_profile/tests/test-files/good/Elm-" ++ elmVersion ++ "//*." ++ gitSha ++ ".prof") %> \out -> do
            let source = dropDirectory1 $ dropExtensions out <.> "elm"
            let elmFormatProf = "_build" </> "bin" </> "elm-format" </> "profile" </> "elm-format" <.> exe
            need [ elmFormatProf, source ]
            cmd_ elmFormatProf source "--output" "/dev/null" ("--elm-version=" ++ elmVersion) "+RTS" "-p" ("-po" ++ (out -<.> ""))

        ("_build/hpc/run/integration-tests/tests/test-files/*/Elm-" <> elmVersion <> "//*.tix") %> \out -> do
            let sourceBase = nTimes 4 dropDirectory1 out
            let exampleType = takeDirectory1 $ nTimes 2 dropDirectory1 sourceBase
            let elmFormatHpc = "_build" </> "bin" </> "elm-format" </> "coverage" </> "elm-format" <.> exe
            let sourceExt = case exampleType of
                  "from-json" -> "json"
                  _ -> "elm"
            let source = sourceBase -<.> sourceExt
            need [ elmFormatHpc, source ]
            whenM (doesFileExist out)
                (liftIO $ removeFile out)
            case exampleType of
                "bad" ->  do
                    (Exit _) <- cmd (AddEnv "HPCTIXFILE" out) elmFormatHpc source "--output" "/dev/null" ("--elm-version=" ++ elmVersion)
                    return ()
                "from-json" -> do
                    cmd_ (AddEnv "HPCTIXFILE" out) elmFormatHpc "--from-json" source
                _ -> do
                    cmd_ (AddEnv "HPCTIXFILE" out) elmFormatHpc source "--output" "/dev/null" ("--elm-version=" ++ elmVersion)

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_formatted") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            cmd_ elmFormat source "--output" out ("--elm-version=" ++ elmVersion)

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_stderr") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            (Stderr stderr, Exit _) <- cmd (FileStdin source) BinaryPipes elmFormat "--stdin" ("--elm-version=" ++ elmVersion)
            cmd_ (FileStdout out) (Stdin stderr) BinaryPipes "tr" [ "-d", "\r" ]

    "_build/tests//*.elm_from_json" %> \out -> do
        let source = dropDirectory1 $ out -<.> "json"
        need [ elmFormat, source ]
        cmd_ (FileStdout out) elmFormat "--from-json" source

    "_build/tests//*.elm_matches" %> \out -> do
        let actual = out -<.> "elm_formatted"
        let expected = dropDirectory1 $ out -<.> "elm"
        need [ actual, expected ]
        cmd_ "diff" "-u" actual expected
        writeFile' out ""

    "_build/tests//*.elm_transform_matches" %> \out -> do
        let actual = out -<.> "elm_formatted"
        let expected = dropDirectory1 $ out -<.> "formatted.elm"
        need [ actual, expected ]
        cmd_ "diff" "-u" actual expected
        writeFile' out ""

    "_build/tests//*.elm_bad_matches" %> \out -> do
        let actual = out -<.> "elm_stderr"
        let expected = dropDirectory1 $ out -<.> "output.txt"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""

    "_build/tests//*.elm_from_json_matches" %> \out -> do
        let actual = out -<.> "elm_from_json"
        let expected = dropDirectory1 $ out -<.> "elm"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""


    -- JSON files

    "_build/integration-tests/good-json.ok" %> \out -> do
        jsonFiles <- getDirectoryFiles ""
            [ "tests/test-files/good//*.json"
            ]
        let oks = ["_build" </> f -<.> "json_matches" | f <- jsonFiles]
        need oks
        writeFile' out (unlines jsonFiles)

    "_build/tests//*.json_formatted" %> \out -> do
        let source = dropDirectory1 $ out -<.> "elm"
        need [ elmFormat, source ]
        (Stdout rawJson) <- cmd (FileStdin source) elmFormat "--elm-version=0.19" "--stdin" "--json"
        (Stdout formattedJson) <- cmd (Stdin rawJson) "jq" "--indent" "4" "--sort-keys" "--ascii-output" "."
        writeFileChanged out formattedJson

    "_build/tests//*.json_matches" %> \out -> do
        let actual = out -<.> "json_formatted"
        let expected = dropDirectory1 $ out -<.> "json"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""

    "_build/integration-tests/good-json-roundtrip.ok" %> \out -> do
        jsonFiles <- getDirectoryFiles ""
            [ "tests/test-files/good//*.json"
            ]
        let oks = ["_build" </> f -<.> "json_roundtrip_matches" | f <- jsonFiles]
        need oks
        writeFile' out (unlines jsonFiles)

    "_build/tests//*.json_roundtrip_formatted" %> \out -> do
        let source = out -<.> "json_formatted"
        need [ elmFormat, source ]
        cmd_ (FileStdout out) elmFormat "--elm-version=0.19" "--from-json" source

    "_build/tests//*.json_roundtrip_matches" %> \out -> do
        let actual = out -<.> "json_roundtrip_formatted"
        let expected = dropDirectory1 $ out -<.> "json-roundtrip"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""
