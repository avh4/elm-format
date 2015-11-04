module Test.Property where

import Data.Char
import Test.HUnit (assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck


propertyTests :: Test
propertyTests =
    testGroup "example test group"
    [ testProperty "example QuickCheck test" ((\s -> s == s) :: [Char] -> Bool)
    , testCase "example HUnit test" $ assertEqual "example assertion" 2 (1+1)
    ]
