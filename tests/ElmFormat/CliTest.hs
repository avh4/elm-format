{-# LANGUAGE FlexibleInstances #-}
module ElmFormat.CliTest where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit

import ElmFormat.World

import qualified Control.Monad.State.Lazy as State
import qualified Data.Map.Strict as Dict
import qualified ElmFormat.Cli as Cli


fakeParse :: String -> Either String ()
fakeParse "fake input" = Right ()
fakeParse input = Left $ "Bad input: " ++ input

fakeRender :: () -> String
fakeRender () = "fake output"


data TestWorldState =
    TestWorldState
        { filesystem :: Dict.Map String String
        }
    deriving (Show)


instance World (State.State TestWorldState) where
    readFile path =
        do
            state <- State.get
            -- TODO: what does IO do when the file doesn't exist?
            return $ Dict.findWithDefault "<file did not exist>" path (filesystem state)
    writeFile path content =
        do
            state <- State.get
            State.put $ state { filesystem = Dict.insert path content (filesystem state) }


testWorld :: [(String, String)] -> TestWorldState
testWorld files =
      TestWorldState
          { filesystem = Dict.fromList files
          }


elmFormat :: [String] -> TestWorldState -> TestWorldState
elmFormat args input =
    State.execState
        (Cli.main fakeParse fakeRender args)
        input


assertOutput :: [(String, String)] -> TestWorldState -> Assertion
assertOutput expectedFiles context =
    assertBool
        ("Expected filesystem to contain: " ++ show expectedFiles ++ "\nActual: " ++ show context)
        (all (\(k,v) -> Dict.lookup k (filesystem context) == Just v) expectedFiles)


tests :: TestTree
tests =
    testGroup "ElmFormat.Cli"
    [ testWorld [ ("file.elm", "fake input") ]
        |> elmFormat [ "file.elm" ]
        |> assertOutput
            [ ("file.elm", "fake output") ]
        |> testCase "format a single file in place"
    ]
