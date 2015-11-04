module Test.Property where

import Elm.Utils ((|>))

import Data.Char
import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Text.Lazy as LazyText
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render


assertStringToString :: String -> Assertion
assertStringToString source =
    let
        source' = LazyText.pack source

        result =
            Parse.parse source'
                |> Parse.toEither
                |> fmap Render.render
    in
        assertEqual "" (Right source') result


propertyTests :: Test
propertyTests =
    testGroup "example test group"
    [ testProperty "example QuickCheck test" ((\s -> s == s) :: [Char] -> Bool)

    , testCase "simple round trip" $
        assertStringToString "module Main (..) where\n\nfoo =\n    8\n"
    ]
