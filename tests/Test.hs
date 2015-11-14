module Main where

import Test.Framework

import Test.Property
import qualified BoxTest
import qualified Parse.ExpressionTest


main :: IO ()
main =
  defaultMain
    [ propertyTests
    , BoxTest.tests
    , Parse.ExpressionTest.tests
    ]
