{-# LANGUAGE FlexibleInstances #-}
module ElmFormat.TestWorld where

import ElmFormat.World
import Test.Tasty.HUnit (Assertion, assertBool)

import qualified Control.Monad.State.Lazy as State
import qualified Data.Map.Strict as Dict


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


exec = State.execState


assertOutput :: [(String, String)] -> TestWorldState -> Assertion
assertOutput expectedFiles context =
    assertBool
        ("Expected filesystem to contain: " ++ show expectedFiles ++ "\nActual: " ++ show context)
        (all (\(k,v) -> Dict.lookup k (filesystem context) == Just v) expectedFiles)
