module ElmFormat.CliTest where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit

import ElmFormat.World
import ElmFormat.TestWorld

import qualified ElmFormat.Cli as Cli


fakeParse :: String -> Either String ()
fakeParse "good input" = Right ()
fakeParse input = Left $ "Bad input: " ++ input

fakeRender :: () -> String
fakeRender () = "good output"


elmFormat :: [String] -> TestWorldState -> TestWorldState
elmFormat args input =
    exec
        (Cli.main fakeParse fakeRender args)
        input


world :: TestWorldState
world =
    testWorld
        [ ("good.elm", "good input") ]


tests :: TestTree
tests =
    testGroup "ElmFormat.Cli"
    [ elmFormat [ "good.elm" ] world
        |> assertOutput
            [ ("good.elm", "good output") ]
        |> testCase "format a single file in place"
    ]
