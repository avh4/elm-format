module Integration.CliTest (tests) where

import Elm.Utils ((|>))
import Test.Tasty
import Test.Tasty.HUnit
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
        ]


world :: TestWorld
world =
    TestWorld.init
        |> TestWorld.installProgram "elm-format" (ElmFormat.main' ElmVersion.Elm_0_18)
        |> TestWorld.installProgram "elm-format-0.18" (ElmFormat.main' ElmVersion.Elm_0_18)
        |> TestWorld.installProgram "elm-format-0.17" (ElmFormat.main' ElmVersion.Elm_0_17)
