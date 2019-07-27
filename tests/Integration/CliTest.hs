module Integration.CliTest (tests) where

import Prelude hiding (readFile)
import Elm.Utils ((|>))
import Test.Tasty
import Test.Tasty.HUnit
import ElmFormat.World (readFile)
import ElmFormat.TestWorld (TestWorld, run, expectExit)
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
        ]


assertPrefix :: String -> String -> Assertion
assertPrefix prefix str =
    assertEqual ("should start with " ++ prefix) prefix (take (length prefix) str)


world :: TestWorld
world =
    TestWorld.init
        |> TestWorld.installProgram "elm-format" ElmFormat.main'
        |> TestWorld.installProgram "elm-format-xxx" (ElmFormat.main'' "x.x.x" Nothing)
