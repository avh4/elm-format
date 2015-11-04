module Test.Property where

import Elm.Utils ((|>))

import Data.Char
import Test.HUnit (assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Text.Lazy as LazyText
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render

propertyTests :: Test
propertyTests =
    testGroup "example test group"
    [ testProperty "example QuickCheck test" ((\s -> s == s) :: [Char] -> Bool)

    , testCase "simple round trip" $
        assertEqual "" (Right $ LazyText.pack "module Main (..) where\n\nfoo =\n    8\n") $
            (Parse.parse (LazyText.pack "module Main (..) where\n\nfoo =\n    8\n") |> Parse.toEither |> fmap Render.render)
    ]
