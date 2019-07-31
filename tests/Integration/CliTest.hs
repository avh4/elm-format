module Integration.CliTest (tests) where

import Prelude hiding (readFile)
import Elm.Utils ((|>))
import Test.Tasty
import Test.Tasty.HUnit
import ElmFormat.World (readFile)
import ElmFormat.TestWorld (TestWorld, run, expectExit, goldenExitStdout, expectFileContents)
import qualified ElmFormat
import qualified ElmFormat.TestWorld as TestWorld


tests :: TestTree
tests =
    testGroup "CLI"
        [ testCase "usage" $
            world
                |> run "elm-format" [ "--help" ]
                |> expectExit 0
        , world
            |> run "elm-format-xxx" []
            |> TestWorld.goldenStdout "usage instructions"
                "tests/usage.stdout"
        , testCase "simple file" $ world
            |> TestWorld.uploadFile "test.elm" "module Main exposing (..)\nf = 1"
            |> run "elm-format" ["test.elm", "--output", "out.elm", "--elm-version=0.19"]
            |> TestWorld.eval (readFile "out.elm")
            |> assertPrefix "module Main"
        , world
            |> TestWorld.queueStdin "syntax error:True"
            |> run "elm-format" ["--stdin", "--elm-version=0.19"]
            |> TestWorld.goldenStderr "using --stdin writes errors to stderr" "tests/stdin-error.stderr"
        , testGroup "auto-detects Elm version"
            [ testCase "for Elm 0.19 applications" $ world
                |> TestWorld.uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\u{2000}'\n"
                |> TestWorld.uploadFile "elm.json" "{\"elm-version\": \"0.19.0\"}"
                |> run "elm-format" ["test.elm", "--validate"]
                |> expectExit 0
            , testCase "for Elm 0.19 packages" $ world
                |> TestWorld.uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\u{2000}'\n"
                |> TestWorld.uploadFile "elm.json" "{\"elm-version\": \"0.19.0 <= v < 0.20.0\"}"
                |> run "elm-format" ["test.elm", "--validate"]
                |> expectExit 0
            , testCase "for Elm 0.18" $ world
                |> TestWorld.uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\x2000'\n"
                |> TestWorld.uploadFile "elm-package.json" "{\"elm-version\": \"0.18.0 <= v < 0.19.0\"}"
                |> run "elm-format" ["test.elm", "--validate"]
                |> expectExit 0
            , testCase "default to Elm 0.19" $ world
                |> TestWorld.uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\u{2000}'\n"
                |> run "elm-format" ["test.elm", "--validate"]
                |> expectExit 0
            ]
        , testGroup "ways to run"
            [ goldenExitStdout "elm-format --help" 0 "tests/usage.stdout" $ world
                |> run "elm-format-xxx" [ "--help" ]
            , goldenExitStdout "elm-format -h" 0 "tests/usage.stdout" $ world
                |> run "elm-format-xxx" [ "-h" ]
            , goldenExitStdout "elm-format (no args)" 1 "tests/usage.stdout" $ world
                |> run "elm-format-xxx" []
            , testCase "elm-format INPUT --yes does change the file" $ world
                |> TestWorld.uploadFile "file.elm" unformatted_elm
                |> run "elm-format" [ "file.elm", "--elm-version=0.19", "--yes" ]
                |> expectFileContents "file.elm" formatted_elm
            , testCase "elm-format INPUT --validate does not change things" $ world
                |> TestWorld.uploadFile "unformatted.elm" unformatted_elm
                |> run "elm-format" [ "unformatted.elm", "--elm-version=0.19", "--validate" ]
                |> expectFileContents "unformatted.elm" unformatted_elm
            , testCase "elm-format INPUT --validate with unformatted file exits 1" $ world
                |> TestWorld.uploadFile "unformatted.elm" unformatted_elm
                |> run "elm-format" [ "unformatted.elm", "--elm-version=0.19", "--validate" ]
                |> expectExit 1
            , testCase "elm-format INPUT --validate with formatted file exits 0" $ world
                |> TestWorld.uploadFile "formatted.elm" formatted_elm
                |> run "elm-format" [ "formatted.elm", "--elm-version=0.19", "--validate" ]
                |> expectExit 0
            ]
        ]

unformatted_elm :: String
unformatted_elm =
    unlines
        [ "module MyMain exposing (x)"
        , "x = ()"
        ]

formatted_elm :: String
formatted_elm =
    unlines
        [ "module MyMain exposing (x)"
        , ""
        , ""
        , "x ="
        , "    ()"
        ]

assertPrefix :: String -> String -> Assertion
assertPrefix prefix str =
    assertEqual ("should start with " ++ prefix) prefix (take (length prefix) str)


world :: TestWorld
world =
    TestWorld.init
        |> TestWorld.installProgram "elm-format" ElmFormat.main'
        |> TestWorld.installProgram "elm-format-xxx" (ElmFormat.main'' "x.x.x" Nothing)
