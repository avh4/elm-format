import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Control.Monad (forM_)
import qualified System.Directory
import qualified System.Info

data OS = Linux | Mac | Windows

os :: OS
os =
    case (System.Info.os, System.Info.arch) of
        ("linux", "x86_64") -> Linux
        ("darwin", "x86_64") -> Mac
        ("osx", "x86_64") -> Mac
        ("mingw32", "x86_64") -> Windows
        ("win32", "x86_64") -> Windows
        other -> error ("unhandled operating system: " ++ show other)

instance Show OS where
    show Linux = "linux-x64"
    show Mac = "mac-x64"
    show Windows = "win-x64"

zipFormatFor Linux = "tgz"
zipFormatFor Mac = "tgz"
zipFormatFor Windows = "zip"

cabalInstallOs :: String
cabalInstallOs =
    System.Info.arch ++ "-" ++ os
    where
        os =
            case System.Info.os of
                "darwin" -> "osx"
                "mingw32" -> "windows"
                o -> o

main :: IO ()
main = do
    shakefilesHash <- getHashedShakeVersion [ "Shakefile.hs" ]
    shakeArgs shakeOptions{
      shakeChange = ChangeModtimeAndDigest,
      shakeColor = True,
      shakeVersion = shakefilesHash
    } $ do

    let zipFormat = zipFormatFor os
    let localBinDir = "bin"

    StdoutTrim gitDescribe <- liftIO $ cmd "git" [ "describe", "--abbrev=8", "--match", "[0-9]*", "--always" ]
    StdoutTrim gitSha <- liftIO $ cmd "git" [ "describe", "--always", "--match", "NOT A TAG", "--dirty" ]

    let elmFormat = "_build" </> "elm-format" <.> exe
    let elmFormatDist = "dist-newstyle/build" </> cabalInstallOs </> "ghc-8.8.4/elm-format-0.8.4/x/elm-format/opt/build/elm-format/elm-format" <.> exe
    let shellcheck = localBinDir </> "shellcheck" <.> exe

    want [ "test" ]

    phony "test" $ need
        [ "unit-tests"
        , "integration-tests"
        , "_build/shellcheck.ok"
        ]

    phony "build" $ need [ elmFormat ]
    phony "unit-tests" $ need [ "_build/cabal-test.ok" ]
    phony "profile" $ need [ "_build/tests/test-files/prof.ok" ]
    phony "dist" $ need [ "dist/elm-format-" ++ gitDescribe ++ "-" ++ show os <.> zipFormat ]

    phony "dependencies" $ need
        [ "_build/cabal-dependencies.ok"
        , "_build/cabal-test-dependencies.ok"
        , shellcheck
        ]
    phony "dist-dependencies" $ need
        [ "_build/cabal-dependencies.ok"
        ]

    phony "clean" $ do
        removeFilesAfter "dist-newstyle" [ "//*" ]
        removeFilesAfter "_build" [ "//*" ]
        removeFilesAfter ".shake" [ "//*" ]
        removeFilesAfter "."
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
          , "cabal.project"
          , "cabal.project.freeze"
          , "cabal.project.local"
          ]

    "generated/Build_elm_format.hs" %> \out -> do
        alwaysRerun
        writeFileChanged out $ unlines
            [ "module Build_elm_format where"
            , ""
            , "gitDescribe :: String"
            , "gitDescribe = " ++ show (gitDescribe :: String)
            ]

    elmFormat %> \out -> do
        copyFileChanged ("dist-newstyle/build" </> cabalInstallOs </> "ghc-8.8.4/elm-format-0.8.4/x/elm-format/noopt/build/elm-format/elm-format" <.> exe) out

    ("dist-newstyle/build" </> cabalInstallOs </> "ghc-8.8.4/elm-format-0.8.4/x/elm-format/noopt/build/elm-format/elm-format" <.> exe) %> \out -> do
        sourceFiles <- getDirectoryFiles "" sourceFilesPattern
        need sourceFiles
        need generatedSourceFiles
        cmd_ "cabal" "v2-build" "-O0"

    elmFormatDist %> \out -> do
        sourceFiles <- getDirectoryFiles "" sourceFilesPattern
        need sourceFiles
        need generatedSourceFiles
        cmd_ "cabal" "v2-build" "-O2"

    ("_build/dist/" ++ show os ++ "/elm-format" <.> exe) %> \out -> do
        need [ elmFormatDist ]
        cmd_ "strip" "-o" out elmFormatDist

    ("dist/elm-format-" ++ gitDescribe ++ "-" ++ show os <.> "tgz") %> \out -> do
        let binDir = "_build/dist/" ++ show os
        need [ binDir </> "elm-format" <.> exe ]
        cmd_ "tar" "zcvf" out "-C" binDir ("elm-format" <.> exe)

    ("dist/elm-format-" ++ gitDescribe ++ "-" ++ show os <.> "zip") %> \out -> do
        let binDir = "_build/dist/" ++ show os
        let bin = binDir </> "elm-format" <.> exe
        need [ bin ]
        absoluteBinPath <- liftIO $ System.Directory.makeAbsolute bin
        liftIO $  removeFiles "." [ out ]
        cmd_ "7z" "a" "-bb3" "-tzip" "-mfb=258" "-mpass=15" out absoluteBinPath


    --
    -- Haskell tests
    --

    "_build/cabal-test.ok" %> \out -> do
        testFiles <- getDirectoryFiles ""
            [ "tests//*.hs"
            , "tests//*.stdout"
            , "tests//*.stderr"
            ]
        need testFiles
        sourceFiles <- getDirectoryFiles "" sourceFilesPattern
        need sourceFiles
        need generatedSourceFiles
        cmd_ "cabal" "v2-test" "-O0" "--test-show-details=streaming"
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
        (Stdout formattedJson) <- cmd (Stdin rawJson) "jq" "--indent" "4" "--sort-keys" "--ascii-output" "."
        writeFileChanged out formattedJson

    "_build/tests//*.json_matches" %> \out -> do
        let actual = out -<.> "json_formatted"
        let expected = dropDirectory1 $ out -<.> "json"
        need [ actual, expected ]
        cmd_ "diff" "--strip-trailing-cr" "-u" actual expected
        writeFile' out ""


    --
    -- dependencies
    --

    "_build/cabal-dependencies.ok" %> \out -> do
        need
            [ "elm-format.cabal"
            , "cabal.project"
            , "cabal.project.freeze"
            ]
        cmd_ "cabal" [ "v2-build", "--only-dependencies" ]
        writeFile' out ""

    "_build/cabal-test-dependencies.ok" %> \out -> do
        need
            [ "elm-format.cabal"
            , "cabal.project"
            , "cabal.project.freeze"
            ]
        cmd_ "cabal" [ "v2-build", "--only-dependencies", "--enable-tests" ]
        writeFile' out ""


    --
    -- shellcheck
    --

    shellcheck %> \out -> do
        -- install-method is currently needed because Windows doesn't support the default symlink method
        cmd_ "cabal" "v2-install" "ShellCheck" "--installdir" localBinDir "--install-method=copy" "--overwrite-policy=always"

    "_build/shellcheck.ok" %> \out -> do
        scriptFiles <- getDirectoryFiles ""
            [ "tests/run-tests.sh"
            , "package/collect_files.sh"
            , "package/linux/build-in-docker.sh"
            , "package/linux/build-package.sh"
            , "package/mac/build-package.sh"
            , "package/nix/build.sh"
            , "package/win/build-package.sh"
            , "build.sh"
            ]
        let oks = ["_build" </> f <.> "shellcheck.ok" | f <- scriptFiles]
        need oks
        writeFile' out (unlines scriptFiles)

    "_build//*.shellcheck.ok" %> \out -> do
        let script = dropDirectory1 $ dropExtension $ dropExtension out
        need [ shellcheck, script ]
        cmd_ shellcheck script
        writeFile' out ""
