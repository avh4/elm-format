module Parse.ExpressionTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Text.Lazy as LazyText

import Parse.Expression
import Parse.Helpers (IParser, iParse)
import AST.V0_15
import AST.Expression
import AST.Literal
import Reporting.Annotation
import Reporting.Region


assertParse :: (Show a, Eq a) => IParser a -> String -> a -> Assertion
assertParse parser input expected =
    let
        output = iParse parser input
    in
        case output of
            Left err ->
                assertEqual (show err) False True
            Right result ->
                assertEqual input expected result


nowhere = Region (Position 0 0) (Position 0 0)

simple e = A nowhere $ e


tests :: Test
tests =
    testGroup "Parse.Expression"
    [ testCase "boolean (True)" $
        assertParse expr "True" $ simple $ Literal $ Commented [] $ Boolean True
    , testCase "boolean (False)" $
        assertParse expr "False" $ simple $ Literal $ Commented [] $ Boolean False

    , testCase "tuple" $
        assertParse expr "(1,2)" $ simple $ Tuple [simple $ Literal $ Commented [] $ IntNum 1, simple $ Literal $ Commented [] $ IntNum 2] False
    ]
