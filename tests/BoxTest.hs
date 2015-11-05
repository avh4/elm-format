module BoxTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

import Box


assertLineOutput :: String -> Line -> Assertion
assertLineOutput expected actual =
    assertEqual expected (expected ++ "\n") $
        render $ line $ actual


tests :: Test
tests =
    testGroup "ElmFormat.Render.BoxTest"
    [ testCase "keyword" $
        assertLineOutput "module" $ keyword "module"
    , testCase "identifier" $
        assertLineOutput "sqrt" $ identifier "sqrt"
    ]
