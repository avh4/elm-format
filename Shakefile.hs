import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Debug.Trace
import Control.Monad (forM_)

main :: IO ()
main = shakeArgs shakeOptions $ do
    Stdout p1 <- liftIO $ cmd "stack path --local-install-root"
    let stackLocalInstallRoot = takeWhile (/= '\n') p1
    Stdout p2 <- liftIO $ cmd "stack path --local-bin"
    let stackLocalBin = takeWhile (/= '\n') p2

    let elmFormat = stackLocalInstallRoot </> "bin/elm-format" <.> exe
    let shellcheck = stackLocalBin </> "shellcheck" <.> exe

    want [ "test" ]

    phony "test" $ do
        need
            [ "_build/stack-test.ok"
            , "integration-tests"
            , "_build/shellcheck.ok"
            ]

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

    elmFormat %> \out -> do
        sourceFiles <- getDirectoryFiles ""
            [ "src//*.hs"
            , "parser/src//*.hs"
            , "markdown//*.hs"
            , "Setup.hs"
            , "elm-format.cabal"
            , "stack.yaml"
            ]
        need sourceFiles
        cmd_ "stack build"

    --
    -- Haskell tests
    --

    "_build/stack-test.ok" %> \out -> do
        testFiles <- getDirectoryFiles ""
            [ "tests//*.hs"
            , "tests//*.stdout"
            , "tests//*.stderr"
            -- source files
            , "src//*.hs"
            , "parser/src//*.hs"
            , "markdown//*.hs"
            , "Setup.hs"
            , "elm-format.cabal"
            , "stack.yaml"
            ]
        need testFiles
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
            ]

    "_build/run-tests.ok" %> \out -> do
        let script = "tests/run-tests.sh"
        need [ script, elmFormat ]
        testFiles <- getDirectoryFiles ""
            [ "tests/test-files/good/json//*.elm"
            , "tests/test-files/bad//*.elm"
            , "tests/test-files/transform//*.elm"
            , "tests/test-files/directory//*.elm"
            , "tests/test-files/recursive-directory//*.elm"
            , "tests/test-files/*.json"
            ]
        need testFiles
        cmd_ "bash" script
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
            writeFile' out ""

        ("_build/tests/test-files/good/Elm-" ++ elmVersion ++ "//*.elm_formatted") %> \out -> do
            let source = dropDirectory1 $ out -<.> "elm"
            need [ elmFormat, source ]
            cmd_ elmFormat source "--output" out ("--elm-version=" ++ elmVersion)

    "_build/tests//*.elm_matches" %> \out -> do
        let actual = out -<.> "elm_formatted"
        let expected = dropDirectory1 $ out -<.> "elm"
        need [ actual, expected ]
        cmd_ "diff" "-u" actual expected
        writeFile' out ""


    -- JSON files

    "_build/tests/test-files/good-json.ok" %> \out -> do
        jsonFiles <- getDirectoryFiles ""
            [ "tests/test-files/good//*.json"
            ]
        let oks = ["_build" </> f -<.> "json_matches" | f <- jsonFiles]
        need oks
        writeFile' out ""

    "_build/tests//*.json_formatted" %> \out -> do
        let script = "tests/format-json.sh"
        let source = dropDirectory1 $ out -<.> "elm"
        need [ elmFormat, script, source ]
        cmd_ script elmFormat source out

    "_build/tests//*.json_matches" %> \out -> do
        let actual = out -<.> "json_formatted"
        let expected = dropDirectory1 $ out -<.> "json"
        need [ actual, expected ]
        cmd_ "diff" "-u" actual expected
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
            , "package/mac/build-package.sh"
            , "package/linux/build-package.sh"
            ]
        let oks = ["_build" </> f <.> "shellcheck.ok" | f <- scriptFiles]
        need oks
        writeFile' out ""

    "_build//*.shellcheck.ok" %> \out -> do
        let script = traceShowId $ dropDirectory1 $ dropExtension $ dropExtension out
        need [ shellcheck, script ]
        cmd_ shellcheck script
        writeFile' out ""
