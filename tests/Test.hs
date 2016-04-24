module Main where

import Test.Framework

import qualified Test.Property
import qualified Test.Integration
import qualified BoxTest
import qualified Parse.ExpressionTest
import qualified Parse.HelpersTest
import qualified Parse.LiteralTest
import qualified Parse.PatternTest
import qualified Parse.TypeTest
import qualified Parse.TestHelpersTest
import qualified Util.ListTest


main :: IO ()
main =
  defaultMain
    [ Test.Property.propertyTests
    , BoxTest.tests
    , Parse.ExpressionTest.tests
    , Parse.HelpersTest.tests
    , Parse.LiteralTest.tests
    , Parse.PatternTest.tests
    , Parse.TypeTest.tests
    , Parse.TestHelpersTest.tests
    , Util.ListTest.tests
    , Test.Integration.tests
    ]
