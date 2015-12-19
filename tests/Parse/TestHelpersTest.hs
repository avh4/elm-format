module Parse.TestHelpersTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit


import Parse.TestHelpers



tests :: Test
tests =
    testGroup "TestHelpers"
    [ testGroup "generateReplacements"
        [ testCase "empty" $
            assertEqual "" [] $ generateReplacements "a" "b" ""
        , testCase "single match" $
            assertEqual "" ["1b2"] $ generateReplacements "a" "b" "1a2"
        , testCase "multiple matches" $
            assertEqual "" ["b1a2a3a", "a1b2a3a", "a1a2b3a", "a1a2a3b"] $ generateReplacements "a" "b" "a1a2a3a"
        ]
    ]
