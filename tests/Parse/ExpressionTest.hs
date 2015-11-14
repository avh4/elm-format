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
import AST.Variable
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


assertFailure :: (Show a, Eq a) => IParser a -> String -> Assertion
assertFailure parser input =
    let
        output = iParse parser input
    in
        case output of
            Left err ->
                assertEqual (show err) True True
            Right result ->
                assertEqual (show result) True False


nowhere = Region (Position 0 0) (Position 0 0)

simple e = A nowhere $ e


tests :: Test
tests =
    testGroup "Parse.Expression"
    [ testCase "boolean (True)" $
        assertParse expr "True" $ simple $ Literal $ Commented [] $ Boolean True
    , testCase "boolean (False)" $
        assertParse expr "False" $ simple $ Literal $ Commented [] $ Boolean False

    , testCase "var (lowercase)" $
        assertParse expr "foo" $ simple $ Var $ Commented [] $ VarRef "foo"
    , testCase "var (uppercase)" $
        assertParse expr "Bar" $ simple $ Var $ Commented [] $ VarRef "Bar"
    , testCase "var (qualified)" $
        assertParse expr "Bar.Baz.foo" $ simple $ Var $ Commented [] $ VarRef "Bar.Baz.foo"

    , testCase "record access fuction" $
        assertParse expr ".f1" $ simple $ AccessFunction "f1"

    , testCase "negative" $
        assertParse expr "-True" $ simple $ Unary Negative $ simple $ Literal $ Commented [] $ Boolean True
    , testCase "negative (does not apply to '-')" $
        assertFailure expr "--True"
    , testCase "negative (does not apply to '.')" $
        assertFailure expr "-.foo"

    , testCase "tuple" $
        assertParse expr "(1,2)" $ simple $ Tuple [simple $ Literal $ Commented [] $ IntNum 1, simple $ Literal $ Commented [] $ IntNum 2] False
    ]
