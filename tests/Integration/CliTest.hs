module Integration.CliTest (tests) where

import Prelude hiding (readFile)
import Elm.Utils ((|>))
import Test.Tasty
import Test.Tasty.HUnit
import ElmFormat.World (readFile)
import ElmFormat.TestWorld (TestWorld, run, expectExit)
import qualified ElmFormat
import qualified ElmFormat.TestWorld as TestWorld
import qualified ElmVersion


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
            |> run "elm-format" ["test.elm", "--output", "out.elm"]
            |> TestWorld.eval (readFile "out.elm")
            |> assertPrefix "module Main"
        ]


assertPrefix :: String -> String -> Assertion
assertPrefix prefix str =
    assertEqual ("should start with " ++ prefix) prefix (take (length prefix) str)


world :: TestWorld
world =
    TestWorld.init
        |> TestWorld.installProgram "elm-format" (ElmFormat.main' ElmVersion.Elm_0_18)
        |> TestWorld.installProgram "elm-format-0.18" (ElmFormat.main' ElmVersion.Elm_0_18)
        |> TestWorld.installProgram "elm-format-0.17" (ElmFormat.main' ElmVersion.Elm_0_17)
        |> TestWorld.installProgram "elm-format-xxx" (ElmFormat.main'' ElmVersion.Elm_0_18 "x.x.x" Nothing)
