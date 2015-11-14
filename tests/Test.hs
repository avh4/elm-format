module Main where

import Test.Framework

import Test.Property
import qualified BoxTest
import qualified Parse.ExpressionTest
import qualified Parse.LiteralTest


main :: IO ()
main =
  defaultMain
    [ propertyTests
    , BoxTest.tests
    , Parse.ExpressionTest.tests
    , Parse.LiteralTest.tests
    ]
