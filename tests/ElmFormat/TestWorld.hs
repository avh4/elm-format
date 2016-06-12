{-# LANGUAGE FlexibleInstances #-}
module ElmFormat.TestWorld where

import ElmFormat.World

import Elm.Utils ((|>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool)
import Test.Tasty.Golden (goldenVsString)

import qualified Control.Monad.State.Lazy as State
import qualified Data.Map.Strict as Dict
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text


data TestWorldState =
    TestWorldState
        { filesystem :: Dict.Map String String
        , stdout :: [String]
        }
    deriving (Show)


fullStdout :: TestWorldState -> String
fullStdout state =
    stdout state |> reverse |> concat


instance World (State.State TestWorldState) where
    readFile path =
        do
            state <- State.get
            case Dict.lookup path (filesystem state) of
                Nothing ->
                    error $ path ++ ": does not exist"

                Just content ->
                    return content

    writeFile path content =
        do
            state <- State.get
            State.put $ state { filesystem = Dict.insert path content (filesystem state) }

    putStrLn string =
        do
            state <- State.get
            State.put $ state { stdout = string : stdout state }

    getProgName =
        return "elm-format"


testWorld :: [(String, String)] -> TestWorldState
testWorld files =
      TestWorldState
          { filesystem = Dict.fromList files
          , stdout = []
          }


exec = State.execState


assertOutput :: [(String, String)] -> TestWorldState -> Assertion
assertOutput expectedFiles context =
    assertBool
        ("Expected filesystem to contain: " ++ show expectedFiles ++ "\nActual: " ++ show context)
        (all (\(k,v) -> Dict.lookup k (filesystem context) == Just v) expectedFiles)


goldenStdout :: String -> FilePath -> TestWorldState -> TestTree
goldenStdout testName goldenFile state =
    goldenVsString
        testName
        goldenFile
        (return $ Text.encodeUtf8 $ Text.pack $ fullStdout state)
