module Main where

import Test.Tasty

import qualified Test.Property
-- import qualified CommonMarkTests
import qualified Integration.CliTest
import qualified Integration.LiteralTest


main :: IO ()
main =
    do
        -- markdownTests <- CommonMarkTests.construct
        defaultMain $ testGroup "elm-format" $
            [ Test.Property.propertyTests
            , Integration.CliTest.tests
            , Integration.LiteralTest.tests
            -- , markdownTests
            ]
