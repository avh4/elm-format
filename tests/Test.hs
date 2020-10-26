module Main where

import Test.Tasty

import qualified Test.Property
import qualified AST.MatchReferencesTest
import qualified BoxTest
-- import qualified CommonMarkTests
import qualified Data.List.ExtraTest
import qualified ElmFormat.ImportInfoTest
import qualified ElmFormat.Render.ElmStructureTest
import qualified Integration.CliTest
import qualified Integration.LiteralTest
import qualified Parse.ExpressionTest
import qualified Parse.HelpersTest
import qualified Parse.LiteralTest
import qualified Parse.PatternTest
import qualified Parse.TypeTest
import qualified Parse.TestHelpersTest


main :: IO ()
main =
    do
        -- markdownTests <- CommonMarkTests.construct
        defaultMain $ testGroup "elm-format" $
            [ Test.Property.propertyTests
            , AST.MatchReferencesTest.tests
            , BoxTest.tests
            , Data.List.ExtraTest.tests
            , ElmFormat.ImportInfoTest.tests
            , ElmFormat.Render.ElmStructureTest.tests
            , Integration.CliTest.tests
            , Integration.LiteralTest.tests
            , Parse.ExpressionTest.tests
            , Parse.HelpersTest.tests
            , Parse.LiteralTest.tests
            , Parse.PatternTest.tests
            , Parse.TypeTest.tests
            , Parse.TestHelpersTest.tests
            -- , markdownTests
            ]
