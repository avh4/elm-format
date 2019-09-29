module Main where

import Test.Tasty

import qualified Test.Property
import qualified BoxTest
-- import qualified CommonMarkTests
import qualified ElmFormat.ImportInfoTest
import qualified ElmFormat.Render.ElmStructureTest
import qualified ElmFormat.Upgrade_0_19Test
import qualified Integration.CliTest
import qualified Integration.LiteralTest
import qualified Parse.ExpressionTest
import qualified Parse.HelpersTest
import qualified Parse.LiteralTest
import qualified Parse.PatternTest
import qualified Parse.TypeTest
import qualified Parse.TestHelpersTest
import qualified Util.ListTest


main :: IO ()
main =
    do
        -- markdownTests <- CommonMarkTests.construct
        defaultMain $ testGroup "elm-format" $
            [ Test.Property.propertyTests
            , BoxTest.tests
            , ElmFormat.ImportInfoTest.tests
            , ElmFormat.Render.ElmStructureTest.tests
            , ElmFormat.Upgrade_0_19Test.tests
            , Integration.CliTest.tests
            , Integration.LiteralTest.tests
            , Parse.ExpressionTest.tests
            , Parse.HelpersTest.tests
            , Parse.LiteralTest.tests
            , Parse.PatternTest.tests
            , Parse.TypeTest.tests
            , Parse.TestHelpersTest.tests
            , Util.ListTest.tests
            -- , markdownTests
            ]
