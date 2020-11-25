module Main where

import Test.Tasty
import Test.Tasty.Hspec (testSpec)

import qualified Test.Property
-- import qualified CommonMarkTests
import qualified Integration.CliTest
import qualified Integration.LiteralTest


main :: IO ()
main =
    do
        -- markdownTests <- CommonMarkTests.construct
        spec <- testSpec "" Integration.CliTest.spec_spec
        defaultMain $ testGroup "elm-format" $
            [ Test.Property.propertyTests
            , spec
            , Integration.LiteralTest.tests
            -- , markdownTests
            ]
