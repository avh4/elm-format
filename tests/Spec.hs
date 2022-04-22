module Main where

import Test.Hspec

import qualified Test.Property
-- import qualified CommonMarkTests
import qualified Integration.CliSpec
import qualified Integration.LiteralSpec


main :: IO ()
main =
    do
        -- markdownTests <- CommonMarkTests.construct
        hspec $ describe "elm-format" $ do
            Test.Property.propertyTests
            Integration.CliSpec.spec
            Integration.LiteralSpec.spec
            -- markdownTests
