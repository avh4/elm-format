import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (forM_)
import qualified System.Directory
import qualified System.Info

import qualified Shakefiles.Haskell
import qualified Shakefiles.Shellcheck
import qualified Shakefiles.Dependencies
import qualified Shakefiles.Signature
import Shakefiles.Extra


main :: IO ()
main = do
    shakefiles <- getDirectoryFilesIO "" [ "Shakefile/src//*.hs" ]
    shakefilesHash <- getHashedShakeVersion shakefiles
    shakeArgs shakeOptions{
      shakeChange = ChangeModtimeAndDigest,
      shakeColor = True,
      shakeVersion = shakefilesHash
    } rules


rules :: Rules ()
rules = do

    StdoutTrim gitDescribe <- liftIO $ cmd "git" [ "describe", "--abbrev=8", "--match", "[0-9]*", "--always" ]
    StdoutTrim gitSha <- liftIO $ cmd "git" [ "describe", "--always", "--match", "NOT A TAG", "--dirty" ]

    let elmFormat = "_build" </> "elm-format" <.> exe

    shellcheck <- Shakefiles.Dependencies.rules

    want [ "test" ]

    phony "test" $ need
        [ "unit-tests"
        , "integration-tests"
        , "shellcheck"
        ]

    phony "build" $ need [ "elm-format" ]
    phony "elm-format" $ need [ elmFormat ]
    phony "unit-tests" $ need
        [ "_build/cabal/elm-format-lib/test.ok"
        , "_build/cabal/elm-format-test-lib/test.ok"
        , "_build/cabal/elm-format/test.ok"
        ]
    phony "profile" $ need [ "_build/tests/test-files/prof.ok" ]
    phony "dist" $ need [ "dist-elm-format" ]
    phonyPrefix "publish-" $ \version ->
        need [ "elm-format-publish-" ++ version ]

    phony "clean" $ do
        removeFilesAfter "dist-newstyle" [ "//*" ]
        removeFilesAfter "_build" [ "//*" ]
        removeFilesAfter ".shake" [ "//*" ]
        removeFilesAfter ".shake.cache" [ "//*" ]
        removeFilesAfter "."
            [ "_input.elm"
            , "_input2.elm"
            , "formatted.elm"
            , "formatted.json"
            , "_stdout.txt"
            ]

    Shakefiles.Signature.rules


    --
    -- build
    --

    "generated/Build_elm_format.hs" %> \out -> do
        alwaysRerun
        writeFileChanged out $ unlines
            [ "module Build_elm_format where"
            , ""
            , "gitDescribe :: String"
            , "gitDescribe = " ++ show (gitDescribe :: String)
            ]

    Shakefiles.Haskell.cabalProject "avh4-lib"
        [ "avh4-lib/avh4-lib.cabal" ]
        [ "avh4-lib/src//*.hs" ]
        []
        [ "avh4-lib/test//*.hs" ]
        []

    Shakefiles.Haskell.cabalProject "elm-format-markdown"
        [ "elm-format-markdown/elm-format-markdown.cabal" ]
        [ "elm-format-markdown/src//*.hs"
        , "elm-format-markdown//*.hs"
        ]
        [] [] []

    Shakefiles.Haskell.cabalProject "elm-format-lib"
        [ "elm-format-lib/elm-format-lib.cabal" ]
        [ "elm-format-lib/src//*.hs" ]
        [ "avh4-lib"
        , "elm-format-markdown"
        ]
        [ "elm-format-lib/test//*.hs" ]
        [ "elm-format-test-lib" ]

    Shakefiles.Haskell.cabalProject "elm-format-test-lib"
        [ "elm-format-test-lib/elm-format-test-lib.cabal" ]
        [ "elm-format-test-lib/src//*.hs" ]
        [ "avh4-lib" ]
        [ "elm-format-test-lib/test//*.hs" ]
        []

    Shakefiles.Haskell.cabalProject "elm-format"
        [ "elm-format.cabal"
        , "generated/Build_elm_format.hs"
        ]
        [ "src//*.hs" ]
        [ "avh4-lib"
        , "elm-format-lib"
        ]
        [ "tests//*.hs"
        , "tests//*.stdout"
        , "tests//*.stderr"
        ]
        [ "elm-format-test-lib" ]

    Shakefiles.Haskell.executable elmFormat "elm-format" gitDescribe


    --
    -- integration tests
    --

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


    Shakefiles.Shellcheck.rules shellcheck
