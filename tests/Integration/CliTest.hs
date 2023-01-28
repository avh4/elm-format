module Integration.CliTest (spec_spec) where

import CommandLine.TestWorld
import Test.Hspec hiding (Success)
import qualified ElmFormat.Cli as ElmFormat
import Data.Text (Text)
import qualified Data.Text as Text


spec_spec :: Spec
spec_spec =
    describe "CLI" $ do
        it "usage" $ do
            ElmFormat.main [ "--help" ]
            expectExit 0

        it "usage instructions" $ do
            elmFormatXxx []
            fmap (golden "tests/usage.stdout") fullStdout

        it "simple file" $ do
            uploadFile "test.elm" "module Main exposing (..)\nf = 1"
            ElmFormat.main ["test.elm", "--output", "out.elm", "--elm-version=0.19"]
            result <- downloadFile "out.elm"
            return $ result `shouldSatisfy` hasPrefix "module Main"

        it "using --stdin writes errors to stderr" $ do
            queueStdin "syntax error:True"
            ElmFormat.main ["--stdin", "--elm-version=0.19"]
            fmap (golden "tests/stdin-error.stderr") fullStderr

        describe "auto-detects Elm version" $ do
            it "for Elm 0.19 applications" $ do
                uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\u{2000}'\n"
                uploadFile "elm.json" "{\"elm-version\": \"0.19.0\"}"
                ElmFormat.main ["test.elm", "--validate"]
                expectExit 0

            it "for Elm 0.19 packages" $ do
                uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\u{2000}'\n"
                uploadFile "elm.json" "{\"elm-version\": \"0.19.0 <= v < 0.20.0\"}"
                ElmFormat.main ["test.elm", "--validate"]
                expectExit 0

            it "for Elm 0.18" $ do
                uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\x2000'\n"
                uploadFile "elm-package.json" "{\"elm-version\": \"0.18.0 <= v < 0.19.0\"}"
                ElmFormat.main ["test.elm", "--validate"]
                expectExit 0

            it "default to Elm 0.19" $ do
                uploadFile "test.elm" "module Main exposing (f)\n\n\nf =\n    '\\u{2000}'\n"
                ElmFormat.main ["test.elm", "--validate"]
                expectExit 0

        describe "ways to run" $ do
            it "elm-format --help" $ do
                elmFormatXxx [ "--help" ]
                goldenExitStdout 0 "tests/usage.stdout"

            it "elm-format -h" $ do
                elmFormatXxx [ "-h" ]
                goldenExitStdout 0 "tests/usage.stdout"

            it "elm-format (no args)" $ do
                elmFormatXxx []
                goldenExitStdout 1 "tests/usage.stdout"

            it "elm-format INPUT --yes does change the file" $ do
                uploadFile "file.elm" unformatted_elm
                ElmFormat.main [ "file.elm", "--elm-version=0.19", "--yes" ]
                expectFileContents "file.elm" formatted_elm

            it "elm-format INPUT --validate does not change things" $ do
                uploadFile "unformatted.elm" unformatted_elm
                ElmFormat.main [ "unformatted.elm", "--elm-version=0.19", "--validate" ]
                expectFileContents "unformatted.elm" unformatted_elm

            it "elm-format INPUT --validate with unformatted file exits 1" $ do
                uploadFile "unformatted.elm" unformatted_elm
                ElmFormat.main [ "unformatted.elm", "--elm-version=0.19", "--validate" ]
                expectExit 1

            it "elm-format INPUT --validate with formatted file exits 0" $ do
                uploadFile "formatted.elm" formatted_elm
                ElmFormat.main [ "formatted.elm", "--elm-version=0.19", "--validate" ]
                expectExit 0


unformatted_elm :: Text
unformatted_elm =
    Text.unlines
        [ "module MyMain exposing (x)"
        , "x = ()"
        ]

formatted_elm :: Text
formatted_elm =
    Text.unlines
        [ "module MyMain exposing (x)"
        , ""
        , ""
        , "x ="
        , "    ()"
        ]


hasPrefix :: String -> Maybe Text -> Bool
hasPrefix prefix =
    maybe
        False
        ((==) prefix . take (length prefix) . Text.unpack)


elmFormatXxx :: [String] -> TestWorld ()
elmFormatXxx = ElmFormat.main' "x.x.x" Nothing
