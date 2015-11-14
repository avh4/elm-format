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
import Reporting.Annotation hiding (map, at)
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

at a b c d = A (Region (Position a b) (Position c d))

intExpr (a,b,c,d) i = at a b c d $ Literal $ Commented [] $ IntNum i


tests :: Test
tests =
    testGroup "Parse.Expression"
    [ testCase "boolean (True)" $
        assertParse expr "True" $ at 1 1 1 5 $ Literal $ Commented [] $ Boolean True
    , testCase "boolean (False)" $
        assertParse expr "False" $ at 1 1 1 6 $ Literal $ Commented [] $ Boolean False

    , testCase "var (lowercase)" $
        assertParse expr "foo" $ at 1 1 1 4 $ Var $ Commented [] $ VarRef "foo"
    , testCase "var (uppercase)" $
        assertParse expr "Bar" $ at 1 1 1 4 $ Var $ Commented [] $ VarRef "Bar"
    , testCase "var (qualified)" $
        -- assertParse expr "Bar.Baz.foo" $ at 1 1 1 12 $ Var $ Commented [] $ VarRef "Bar.Baz.foo"
        assertParse expr "Bar.Baz.foo" $ at 1 5 1 12 $ Var $ Commented [] $ VarRef "Bar.Baz.foo"

    , testCase "record access fuction" $
        assertParse expr ".f1" $ at 1 1 1 4 $ AccessFunction "f1"

    , testCase "negative" $
        assertParse expr "-True" $ at 1 1 1 6 $ Unary Negative $ at 1 2 1 6 $ Literal $ Commented [] $ Boolean True
    , testCase "negative (must not have whitespace)" $
        assertFailure expr "- True"
    , testCase "negative (must not have comment)" $
        assertFailure expr "-{- -}True"
    , testCase "negative (does not apply to '-')" $
        assertFailure expr "--True"
    , testCase "negative (does not apply to '.')" $
        assertFailure expr "-.foo"

    , testCase "range" $
        assertParse expr "[7..9]" $ at 1 1 1 7 $ Range (intExpr (1,2,1,3) 7) (intExpr (1,5,1,6) 9) False
    , testCase "range (whitespace)" $
        assertParse expr "[ 7 .. 9 ] " $ at 1 1 1 11 $ Range (intExpr (1,3,1,4) 7) (intExpr (1,8,1,9) 9) False
    , testCase "range (newlines)" $
        assertParse expr "[\n 7\n ..\n 9\n ]\n" $ at 1 1 5 3 $ Range (intExpr (2,2,2,3) 7) (intExpr (4,2,4,3) 9) True
    , testGroup "range (must be indented)"
        [ testCase "(1) " $ assertFailure expr "[\n7\n ..\n 9\n ]"
        , testCase "(2) " $ assertFailure expr "[\n 7\n..\n 9\n ]"
        , testCase "(3) " $ assertFailure expr "[\n 7\n ..\n9\n ]"
        , testCase "(4) " $ assertFailure expr "[\n 7\n ..\n 9\n]"
        ]

    , testCase "list" $
        assertParse expr "[1,2,3]" $ at 1 1 1 8 $ ExplicitList [intExpr (1,2,1,3) 1, intExpr (1,4,1,5) 2, intExpr (1,6,1,7) 3] False
    , testCase "list (empty)" $
        assertParse expr "[]" $ at 1 1 1 3 $ ExplicitList [] False
    , testCase "list (whitespace)" $
        assertParse expr "[ 1 , 2 , 3 ] " $ at 1 1 1 14 $ ExplicitList [intExpr (1,3,1,4) 1, intExpr (1,7,1,8) 2, intExpr (1,11,1,12) 3] False
    , testCase "list (newlines)" $
        assertParse expr "[\n 1\n ,\n 2\n ,\n 3\n ]\n" $ at 1 1 7 3 $ ExplicitList [intExpr (2,2,2,3) 1, intExpr (4,2,4,3) 2, intExpr (6,2,6,3) 3] True
    , testGroup "list (must be indented)"
        [ testCase "(1)" $ assertFailure expr "[\n1\n ,\n 2\n ]"
        , testCase "(2)" $ assertFailure expr "[\n 1\n,\n 2\n ]"
        , testCase "(3)" $ assertFailure expr "[\n 1\n ,\n2\n ]"
        , testCase "(4)" $ assertFailure expr "[\n 1\n ,\n 2\n]"
        ]

    , testCase "symbolic operator as function" $
        -- assertParse expr "(+)" $ at 1 1 1 4 $ Var $ Commented [] $ OpRef "+"
        assertParse expr "(+)" $ at 1 2 1 3 $ Var $ Commented [] $ OpRef "+"
    , testCase "symbolic operator as function (whitespace)" $
        -- assertParse expr "( + ) " $ at 1 1 1 6 $ Var $ Commented [] $ OpRef "+"
        assertParse expr "( + ) " $ at 1 3 1 4 $ Var $ Commented [] $ OpRef "+"
    , testCase "symbolic operator as function (does not allow newlines)" $
        assertFailure expr "(\n + \n)"

    , testCase "tuple function" $
        assertParse expr "(,,)" $ at 1 1 1 5 $ TupleFunction 3
    , testCase "tuple function (whitespace)" $
        assertParse expr "( , ,) " $ at 1 1 1 7 $ TupleFunction 3
    , testCase "tuple function (newlines)" $
        assertParse expr "(\n ,\n ,)\n" $ at 1 1 3 4 $ TupleFunction 3
    , testGroup "tuple function (must be indented)"
        [ testCase "(1)" $ assertFailure expr "(\n,\n ,)"
        , testCase "(2)" $ assertFailure expr "(\n ,\n,)"
        ]
    , testCase "tuple function (does not allow trailing inner whitespace)" $
        assertFailure expr "(,, )"

    , testCase "parenthesized expression" $
        assertParse expr "(7)" $ at 1 1 1 4 $ Parens (intExpr (1,2,1,3) 7) False
    , testCase "parenthesized expression (whitespace)" $
        assertParse expr "( 7 ) " $ at 1 1 1 6 $ Parens (intExpr (1,3,1,4) 7) False
    , testCase "parenthesized expression (newlines)" $
        assertParse expr "(\n 7\n )\n" $ at 1 1 3 3 $ Parens (intExpr (2,2,2,3) 7) True
    , testGroup "parenthesized expression (must be indented)"
        [ testCase "(1)" $ assertFailure expr "(\n7\n )"
        , testCase "(2)" $ assertFailure expr "(\n 7\n)"
        ]

    , testCase "tuple" $
        assertParse expr "(1,2)" $ at 1 1 1 6 $ Tuple [intExpr (1,2,1,3) 1, intExpr (1,4,1,5) 2] False
    , testCase "tuple (whitespace)" $
        assertParse expr "( 1 , 2 ) " $ at 1 1 1 10 $ Tuple [intExpr (1,3,1,4) 1, intExpr (1,7,1,8) 2] False
    , testCase "tuple (newlines)" $
        assertParse expr "(\n 1\n ,\n 2\n )\n" $ at 1 1 5 3 $ Tuple [intExpr (2,2,2,3) 1, intExpr (4,2,4,3) 2] True
    , testGroup "tuple (must be indented)"
        [ testCase "(1)" $ assertFailure expr "(\n1\n ,\n 2\n )"
        , testCase "(2)" $ assertFailure expr "(\n 1\n,\n 2\n )"
        , testCase "(3)" $ assertFailure expr "(\n 1\n ,\n2\n )"
        , testCase "(4)" $ assertFailure expr "(\n 1\n ,\n 2\n)"
        ]
    ]
