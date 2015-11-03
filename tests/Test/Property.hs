module Test.Property where

import Data.Char
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck


propertyTests :: Test
propertyTests =
    testGroup "Parse/Print Agreement Tests"
    [
        testProperty "FOO" ((\s -> s == s) :: [Char] -> Bool)
    ]
