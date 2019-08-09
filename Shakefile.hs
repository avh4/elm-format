import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (forM_)

main :: IO ()
main = do
    shakefilesHash <- getHashedShakeVersion [ "Shakefile.hs" ]
    shakeArgs shakeOptions{
      shakeChange = ChangeModtimeAndDigest,
      shakeColor = True,
      shakeVersion = shakefilesHash
    } $ do
    StdoutTrim stackLocalInstallRoot <- liftIO $ cmd "stack path --local-install-root"
    StdoutTrim stackLocalBin <- liftIO $ cmd "stack path --local-bin"

    let elmFormat = stackLocalInstallRoot </> "bin/elm-format" <.> exe
    let shellcheck = stackLocalBin </> "shellcheck" <.> exe

    want [ "test" ]

    phony "test" $ do
        need
            [ "stack-test"
            , "integration-tests"
            , "_build/shellcheck.ok"
            ]

    phony "build" $ need [ elmFormat ]
    phony "stack-test" $ need [ "_build/stack-test.ok" ]

    phony "clean" $ do
        cmd_ "stack clean"
        removeFilesAfter "_build" [ "//*" ]
        removeFilesAfter ""
            [ "_input.elm"
            , "_input2.elm"
            , "formatted.elm"
            , "formatted.json"
            , "_stdout.txt"
            ]

    --
    -- build
    --

    let generatedSourceFiles = [ "generated/Build_elm_format.hs" ]
    let sourceFilesPattern =
          [ "src//*.hs"
          , "parser/src//*.hs"
          , "markdown//*.hs"
          , "elm-format.cabal"
          , "stack.yaml"
          ]

    "generated/Build_elm_format.hs" %> \out -> do
        alwaysRerun
        (StdoutTrim gitDescribe) <- cmd "git" [ "describe", "--abbrev=8", "--always" ]
        writeFileChanged out $ unlines
            [ "module Build_elm_format where"
            , ""
            , "gitDescribe :: String"
            , "gitDescribe = " ++ show (gitDescribe :: String)
            ]

    elmFormat %> \out -> do
        sourceFiles <- getDirectoryFiles "" sourceFilesPattern
        need sourceFiles
        need generatedSourceFiles
        cmd_ "stack build --test --no-run-tests"

    --
    -- Haskell tests
    --

    "_build/stack-test.ok" %> \out -> do
        testFiles <- getDirectoryFiles ""
            [ "tests//*.hs"
            , "tests//*.stdout"
            , "tests//*.stderr"
            ]
        need testFiles
        sourceFiles <- getDirectoryFiles "" sourceFilesPattern
        need sourceFiles
        need generatedSourceFiles
        cmd_ "stack test"
        writeFile' out ""


    --
    -- integration tests
    --

    phony "integration-tests" $ do
        need
            [ "_build/run-tests.ok"
            , "_build/tests/test-files/good-json.ok"
            , "_build/tests/test-files/good/Elm-0.19.ok"
            , "_build/tests/test-files/good/Elm-0.18.ok"
            , "_build/tests/test-files/good/Elm-0.17.ok"
            , "_build/tests/test-files/transform/Elm-0.19.ok"
            , "_build/tests/test-files/transform/Elm-0.18.ok"
            , "_build/tests/test-files/transform/Elm-0.17.ok"
            , "_build/tests/test-files/upgrade/Elm-0.19.ok"
            , "_build/tests/test-files/upgrade/Elm-0.18.ok"
            , "_build/tests/test-files/upgrade/Elm-0.17.ok"
            , "_build/tests/test-files/bad/Elm-0.19.ok"
            , "_build/tests/test-files/bad/Elm-0.18.ok"
            , "_build/tests/test-files/bad/Elm-0.17.ok"
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

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_formatted") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            cmd_ elmFormat source "--output" out ("--elm-version=" ++ elmVersion)

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_upgraded") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            cmd_ elmFormat source "--output" out "--upgrade" ("--elm-version=" ++ elmVersion)

        ("_build/tests/test-files/*/Elm-" ++ elmVersion ++ "//*.elm_stderr") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            (Stderr stderr, Exit _) <- cmd (FileStdin source) BinaryPipes elmFormat "--stdin" ("--elm-version=" ++ elmVersion)
            cmd_ (FileStdout out) (Stdin stderr) BinaryPipes "tr" [ "-d", "\r" ]

        ("_build/tests/test-files/transform/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/transform/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_transform_matches" | f <- elmFiles, ( takeExtension $ dropExtension f) /= ".formatted" ]
            need oks
            writeFile' out (unlines oks)

        ("_build/tests/test-files/upgrade/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/upgrade/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_upgrade_matches" | f <- elmFiles, ( takeExtension $ dropExtension f) /= ".formatted" ]
            need oks
            writeFile' out (unlines oks)

        ("_build/tests/test-files/bad/Elm-" ++ elmVersion ++ ".ok") %> \out -> do
            elmFiles <- getDirectoryFiles ""
                [ "tests/test-files/bad/Elm-" ++ elmVersion ++ "//*.elm"
                ]
            let oks = ["_build" </> f -<.> "elm_bad_matches" | f <- elmFiles ]
            need oks
            writeFile' out (unlines elmFiles)

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

    "_build/tests//*.elm_upgrade_matches" %> \out -> do
        let actual = out -<.> "elm_upgraded"
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
        (Stdout formattedJson) <- cmd (Stdin rawJson) "python3" "-mjson.tool" "--sort-keys"
        writeFileChanged out formattedJson

    "_build/tests//*.json_matches" %> \out -> do
        let actual = out -<.> "json_formatted"
        let expected = dropDirectory1 $ out -<.> "json"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""


    --
    -- shellcheck
    --

    shellcheck %> \out -> do
        cmd_ "stack install ShellCheck"

    "_build/shellcheck.ok" %> \out -> do
        scriptFiles <- getDirectoryFiles ""
            [ "tests/run-tests.sh"
            , "package/collect_files.sh"
            , "package/linux/build-in-docker.sh"
            , "package/linux/build-package.sh"
            , "package/mac/build-package.sh"
            , "package/nix/build.sh"
            , "package/win/build-package.sh"
            ]
        let oks = ["_build" </> f <.> "shellcheck.ok" | f <- scriptFiles]
        need oks
        writeFile' out (unlines scriptFiles)

    "_build//*.shellcheck.ok" %> \out -> do
        let script = dropDirectory1 $ dropExtension $ dropExtension out
        need [ shellcheck, script ]
        cmd_ shellcheck script
        writeFile' out ""
