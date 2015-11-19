module Main where

import Test.Framework

import qualified Test.Property
import qualified BoxTest
import qualified Parse.ExpressionTest
import qualified Parse.LiteralTest


main :: IO ()
main =
  defaultMain
    [ Test.Property.propertyTests
    , BoxTest.tests
    , Parse.ExpressionTest.tests
    , Parse.LiteralTest.tests
    ]
