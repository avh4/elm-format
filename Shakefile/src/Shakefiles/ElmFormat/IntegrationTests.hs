module Shakefiles.ElmFormat.IntegrationTests where

import Control.Monad (forM_)
import Development.Shake
import Development.Shake.FilePath

rules :: String -> FilePath -> Rules ()
rules gitSha elmFormat = do
    phony "integration-tests" $
        need
            [ "_build/run-tests.ok"
            , "_build/tests/test-files/good-json.ok"
            , "_build/tests/test-files/good-json-roundtrip.ok"
            , "_build/tests/test-files/good/Elm-0.19.ok"
            , "_build/tests/test-files/good/Elm-0.18.ok"
            , "_build/tests/test-files/good/Elm-0.17.ok"
            , "_build/tests/test-files/transform/Elm-0.19.ok"
            , "_build/tests/test-files/transform/Elm-0.18.ok"
            , "_build/tests/test-files/transform/Elm-0.17.ok"
            , "_build/tests/test-files/bad/Elm-0.19.ok"
            , "_build/tests/test-files/bad/Elm-0.18.ok"
            , "_build/tests/test-files/bad/Elm-0.17.ok"
            , "_build/tests/test-files/from-json/Elm-0.19.ok"
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
        need testFiles
        cmd_ ("bash" <.> exe) script elmFormat
        writeFile' out ""

    "_build/tests/test-files/prof.ok" %> \out -> do
        let oks =
              [ "_build/tests/test-files/good/Elm-0.17/prof.ok"
              , "_build/tests/test-files/good/Elm-0.18/prof.ok"
              , "_build/tests/test-files/good/Elm-0.19/prof.ok"
              ]
        need oks
        writeFile' out (unlines oks)


    -- Elm files

    let elmVersions = [ "0.17", "0.18", "0.19" ]

    forM_ elmVersions $ \elmVersion -> do
        ("_build/tests/test-files/good/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/good/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_matches" | f <- elmFiles]
            need oks
            writeFile' out (unlines elmFiles)

        ("_build/tests/test-files/good/Elm-" ++ elmVersion ++ "/prof.ok") %> \out -> do
            alwaysRerun
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/good/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_profile" </> f -<.> (gitSha ++ ".prof") | f <- elmFiles]
            need oks
            writeFile' out (unlines oks)

        ("_profile/tests/test-files/good/Elm-" ++ elmVersion ++ "//*." ++ gitSha ++ ".prof") %> \out -> do
            let source = dropDirectory1 $ dropExtensions out <.> "elm"
            need [ "_build/bin/elm-format-prof", source ]
            cmd_ "_build/bin/elm-format-prof" source "--output" "/dev/null" ("--elm-version=" ++ elmVersion) "+RTS" "-p" ("-po" ++ (out -<.> ""))

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_formatted") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            cmd_ elmFormat source "--output" out ("--elm-version=" ++ elmVersion)

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_stderr") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            (Stderr stderr, Exit _) <- cmd (FileStdin source) BinaryPipes elmFormat "--stdin" ("--elm-version=" ++ elmVersion)
            cmd_ (FileStdout out) (Stdin stderr) BinaryPipes "tr" [ "-d", "\r" ]

        ("_build/tests/test-files/transform/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/transform/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_transform_matches" | f <- elmFiles, takeExtension (dropExtension f) /= ".formatted" ]
            need oks
            writeFile' out (unlines oks)

        ("_build/tests/test-files/bad/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/bad/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_bad_matches" | f <- elmFiles ]
            need oks
            writeFile' out (unlines elmFiles)

        ("_build/tests/test-files/from-json/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            jsonFiles <- getDirectoryFiles ""
                [ "tests/test-files/from-json/Elm-" ++ elmVersion ++ "//*.json"
                ]
            let oks = ["_build" </> f -<.> "elm_from_json_matches" | f <- jsonFiles ]
            need oks
            writeFile' out (unlines oks)

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

    "_build/tests/test-files/good-json.ok" %> \out -> do
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

    "_build/tests/test-files/good-json-roundtrip.ok" %> \out -> do
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
