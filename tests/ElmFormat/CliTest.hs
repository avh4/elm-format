module ElmFormat.CliTest where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit

import ElmFormat.World
import ElmFormat.TestWorld

import qualified ElmFormat.Cli as Cli


fakeParse :: String -> Either String ()
fakeParse "fake input" = Right ()
fakeParse input = Left $ "Bad input: " ++ input

fakeRender :: () -> String
fakeRender () = "fake output"


elmFormat :: [String] -> TestWorldState -> TestWorldState
elmFormat args input =
    exec
        (Cli.main fakeParse fakeRender args)
        input


tests :: TestTree
tests =
    testGroup "ElmFormat.Cli"
    [ testWorld [ ("file.elm", "fake input") ]
        |> elmFormat [ "file.elm" ]
        |> assertOutput
            [ ("file.elm", "fake output") ]
        |> testCase "format a single file in place"
    ]
