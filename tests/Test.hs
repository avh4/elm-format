module Main where

import Test.Framework

import qualified Test.Property
import qualified BoxTest
import qualified Parse.ExpressionTest
import qualified Parse.LiteralTest
import qualified Parse.PatternTest
import qualified Parse.TestHelpersTest
import qualified Util.ListTest


main :: IO ()
main =
  defaultMain
    [ Test.Property.propertyTests
    , BoxTest.tests
    , Parse.ExpressionTest.tests
    , Parse.LiteralTest.tests
    , Parse.PatternTest.tests
    , Parse.TestHelpersTest.tests
    , Util.ListTest.tests
    ]
