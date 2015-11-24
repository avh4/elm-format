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
import qualified AST.Pattern as P
import AST.Variable
import Reporting.Annotation hiding (map, at)
import Reporting.Region
import Text.Parsec.Char (string)

import Parse.TestHelpers


commentedIntExpr (a,b,c,d) preComment postComment i =
    Commented [BlockComment [preComment]] [BlockComment [postComment]] $ at a b c d  $ Literal $ IntNum i

commentedIntExpr' (a,b,c,d) preComment i =
    Commented [BlockComment [preComment]] [] $ at a b c d  $ Literal $ IntNum i


intExpr (a,b,c,d) i = at a b c d $ Literal $ IntNum i

intExpr' (a,b,c,d) i =
    Commented [] [] $ at a b c d  $ Literal $ IntNum i


tests :: Test
tests =
    testGroup "Parse.Expression"
    [ testCase "boolean (True)" $
        assertParse expr "True" $ at 1 1 1 5 $ Literal $ Boolean True
    , testCase "boolean (False)" $
        assertParse expr "False" $ at 1 1 1 6 $ Literal $ Boolean False

    , testCase "var (lowercase)" $
        assertParse expr "foo" $ at 1 1 1 4 $ Var $ VarRef "foo"
    , testCase "var (uppercase)" $
        assertParse expr "Bar" $ at 1 1 1 4 $ Var $ VarRef "Bar"
    , testCase "var (qualified)" $
        assertParse expr "Bar.Baz.foo" $ at 1 1 1 12 $ Var $ VarRef "Bar.Baz.foo"

    , testCase "record access fuction" $
        assertParse expr ".f1" $ at 1 1 1 4 $ AccessFunction "f1"

    , testCase "negative" $
        assertParse expr "-True" $ at 1 1 1 6 $ Unary Negative $ at 1 2 1 6 $ Literal $ Boolean True
    , testCase "negative (must not have whitespace)" $
        assertFailure expr "- True"
    , testCase "negative (must not have comment)" $
        assertFailure expr "-{- -}True"
    , testCase "negative (does not apply to '-')" $
        assertFailure expr "--True"
    , testCase "negative (does not apply to '.')" $
        assertFailure expr "-.foo"

    , testCase "range" $
        assertParse expr "[7..9]" $ at 1 1 1 7 $ Range (intExpr' (1,2,1,3) 7) (intExpr' (1,5,1,6) 9) False
    , testCase "range (whitespace)" $
        assertParse expr "[ 7 .. 9 ]" $ at 1 1 1 11 $ Range (intExpr' (1,3,1,4) 7) (intExpr' (1,8,1,9) 9) False
    , testCase "range (comments)" $
        assertParse expr "[{-A-}7{-B-}..{-C-}9{-D-}]" $ at 1 1 1 27 $ Range (commentedIntExpr (1,7,1,8) "A" "B" 7) (commentedIntExpr (1,20,1,21) "C" "D" 9) False
    , testCase "range (newlines)" $
        assertParse expr "[\n 7\n ..\n 9\n ]" $ at 1 1 5 3 $ Range (intExpr' (2,2,2,3) 7) (intExpr' (4,2,4,3) 9) True
    , testGroup "range (must be indented)"
        [ testCase "(1)" $ assertFailure expr "[\n7\n ..\n 9\n ]"
        , testCase "(2)" $ assertFailure expr "[\n 7\n..\n 9\n ]"
        , testCase "(3)" $ assertFailure expr "[\n 7\n ..\n9\n ]"
        , testCase "(4)" $ assertFailure expr "[\n 7\n ..\n 9\n]"
        ]

    , testCase "list" $
        assertParse expr "[1,2,3]" $ at 1 1 1 8 $ ExplicitList [intExpr' (1,2,1,3) 1, intExpr' (1,4,1,5) 2, intExpr' (1,6,1,7) 3] False
    , testCase "list (single element)" $
        assertParse expr "[1]" $ at 1 1 1 4 $ ExplicitList [intExpr' (1,2,1,3) 1] False
    , testCase "list (empty)" $
        assertParse expr "[]" $ at 1 1 1 3 $ ExplicitList [] False
    , testCase "list (whitespace)" $
        assertParse expr "[ 1 , 2 , 3 ]" $ at 1 1 1 14 $ ExplicitList [intExpr' (1,3,1,4) 1, intExpr' (1,7,1,8) 2, intExpr' (1,11,1,12) 3] False
    , testCase "list (comments)" $
        assertParse expr "[{-A-}1{-B-},{-C-}2{-D-},{-E-}3{-F-}]" $ at 1 1 1 38 $ ExplicitList [commentedIntExpr (1,7,1,8) "A" "B" 1, commentedIntExpr (1,19,1,20) "C" "D" 2, commentedIntExpr (1,31,1,32) "E" "F" 3] False
    , testCase "list (newlines)" $
        assertParse expr "[\n 1\n ,\n 2\n ,\n 3\n ]" $ at 1 1 7 3 $ ExplicitList [intExpr' (2,2,2,3) 1, intExpr' (4,2,4,3) 2, intExpr' (6,2,6,3) 3] True
    , testGroup "list (must be indented)"
        [ testCase "(1)" $ assertFailure expr "[\n1\n ,\n 2\n ]"
        , testCase "(2)" $ assertFailure expr "[\n 1\n,\n 2\n ]"
        , testCase "(3)" $ assertFailure expr "[\n 1\n ,\n2\n ]"
        , testCase "(4)" $ assertFailure expr "[\n 1\n ,\n 2\n]"
        ]

    , testCase "binary operator" $
        assertParse expr "7+8<<>>9" $ at 1 1 1 9 $ Binops (intExpr (1,1,1,2) 7) [(Commented [] [] $ OpRef "+", intExpr' (1,3,1,4) 8), (Commented [] [] $ OpRef "<<>>", intExpr' (1,8,1,9) 9)] False
    , testCase "binary operator (named function)" $
        assertParse expr "7`plus`8`shift`9" $ at 1 1 1 17 $ Binops (intExpr (1,1,1,2) 7) [(Commented [] [] $ VarRef "plus", intExpr' (1,8,1,9) 8), (Commented [] [] $ VarRef "shift", intExpr' (1,16,1,17) 9)] False
    , testCase "binary operator (whitespace)" $
        assertParse expr "7 + 8 <<>> 9" $ at 1 1 1 13 $ Binops (intExpr (1,1,1,2) 7) [(Commented [] [] $ OpRef "+", intExpr' (1,5,1,6) 8), (Commented [] [] $ OpRef "<<>>", intExpr' (1,12,1,13) 9)] False
    , testCase "binary operator (comments)" $
        assertParse expr "7{-A-}+{-B-}8{-C-}<<>>{-D-}9" $ at 1 1 1 29 $ Binops (intExpr (1,1,1,2) 7) [(Commented [BlockComment ["A"]] [] $ OpRef "+", commentedIntExpr' (1,13,1,14) "B" 8), (Commented [BlockComment ["C"]] [] $ OpRef "<<>>", commentedIntExpr' (1,28,1,29) "D" 9)] False
    , testCase "binary operator (newlines)" $
        assertParse expr "7\n +\n 8\n <<>>\n 9" $ at 1 1 5 3 $ Binops (intExpr (1,1,1,2) 7) [(Commented [] [] $ OpRef "+", intExpr' (3,2,3,3) 8), (Commented [] [] $ OpRef "<<>>", intExpr' (5,2,5,3) 9)] True
    , testGroup "binary operator (must be indented)" $
        [ testCase "(1)" $ assertFailure expr "7\n+\n 8\n <<>>\n 9"
        , testCase "(2)" $ assertFailure expr "7\n +\n8\n <<>>\n 9"
        , testCase "(3)" $ assertFailure expr "7\n +\n 8\n<<>>\n 9"
        , testCase "(4)" $ assertFailure expr "7\n +\n 8\n <<>>\n9"
        ]

    , testCase "symbolic operator as function" $
        assertParse expr "(+)" $ at 1 1 1 4 $ Var $ OpRef "+"
    , testCase "symbolic operator as function (whitespace)" $
        assertParse expr "( + )" $ at 1 1 1 6 $ Var $ OpRef "+"
    , testCase "symbolic operator as function (does not allow newlines)" $
        assertFailure expr "(\n + \n)"

    , testCase "tuple function" $
        assertParse expr "(,,)" $ at 1 1 1 5 $ TupleFunction 3
    , testCase "tuple function (whitespace)" $
        assertParse expr "( , ,)" $ at 1 1 1 7 $ TupleFunction 3
    , testCase "tuple function (newlines)" $
        assertParse expr "(\n ,\n ,)" $ at 1 1 3 4 $ TupleFunction 3
    , testGroup "tuple function (must be indented)"
        [ testCase "(1)" $ assertFailure expr "(\n,\n ,)"
        , testCase "(2)" $ assertFailure expr "(\n ,\n,)"
        ]
    , testCase "tuple function (does not allow trailing inner whitespace)" $
        assertFailure expr "(,, )"

    , testCase "parenthesized expression" $
        assertParse expr "(7)" $ at 1 1 1 4 $ Parens (intExpr (1,2,1,3) 7) False
    , testCase "parenthesized expression (whitespace)" $
        assertParse expr "( 7 )" $ at 1 1 1 6 $ Parens (intExpr (1,3,1,4) 7) False
    , testCase "parenthesized expression (newlines)" $
        assertParse expr "(\n 7\n )" $ at 1 1 3 3 $ Parens (intExpr (2,2,2,3) 7) True
    , testGroup "parenthesized expression (must be indented)"
        [ testCase "(1)" $ assertFailure expr "(\n7\n )"
        , testCase "(2)" $ assertFailure expr "(\n 7\n)"
        ]

    , testCase "case" $
        assertParse expr "case 9 of\n 1->10\n _->20" $ at 1 1 3 7 $ Case (intExpr (1,6,1,7) 9, False) [([], at 2 2 2 3 $ P.Literal $ IntNum 1, [], intExpr (2,5,2,7) 10), ([], at 3 2 3 3 $ P.Anything, [], intExpr (3,5,3,7) 20)]
    , testCase "case (no newline after 'of')" $
        assertParse expr "case 9 of 1->10\n          _->20" $ at 1 1 2 16 $ Case (intExpr (1,6,1,7) 9, False) [([], at 1 11 1 12 $ P.Literal $ IntNum 1, [], intExpr (1,14,1,16) 10), ([], at 2 11 2 12 $ P.Anything, [], intExpr (2,14,2,16) 20)]
    , testCase "case (whitespace)" $
        assertParse expr "case 9 of\n 1 -> 10\n _ -> 20" $ at 1 1 3 9 $ Case (intExpr (1,6,1,7) 9, False) [([], at 2 2 2 3 $ P.Literal $ IntNum 1, [], intExpr (2,7,2,9) 10), ([], at 3 2 3 3 $ P.Anything, [], intExpr (3,7,3,9) 20)]
    , testCase "case (comments)" $
        assertParse expr "case{-A-}9{-B-}of{-C-}\n{-D-}1{-E-}->{-F-}10{-G-}\n{-H-}_{-I-}->{-J-}20" $ at 1 1 3 21 $ Case (intExpr (1,10,1,11) 9, False) [([BlockComment ["C"], BlockComment ["D"]], at 2 6 2 7 $ P.Literal $ IntNum 1, [BlockComment ["F"]], intExpr (2,19,2,21) 10), ([BlockComment ["G"], BlockComment ["H"]], at 3 6 3 7 $ P.Anything, [BlockComment ["J"]], intExpr (3,19,3,21) 20)] -- TODO: handle comments A, B, E, I, and don't allow K
    , testCase "case (newlines)" $ -- TODO: should be able to add a newline at the end
        assertParse expr "case\n 9\n of\n 1\n ->\n 10\n _\n ->\n 20" $ at 1 1 9 4 $ Case (intExpr (2,2,2,3) 9, True) [([], at 4 2 4 3 $ P.Literal $ IntNum 1, [], intExpr (6,2,6,4) 10), ([], at 7 2 7 3 $ P.Anything, [], intExpr (9,2,9,4) 20)]
    , testCase "case (should not fail with trailing whitespace)" $
        assertParse (expr >> string "\nX") "case 9 of\n 1->10\n _->20\nX" $ "\nX"
    , testGroup "case (clauses must start at the same column)"
        [ testCase "(1)" $ assertFailure expr "case 9 of\n 1->10\n_->20"
        , testCase "(2)" $ assertFailure expr "case 9 of\n 1->10\n  _->20"
        , testCase "(3)" $ assertFailure expr "case 9 of\n  1->10\n _->20"
        ]
    , testGroup "case (must be indented)"
        [ testCase "(1)" $ assertFailure expr "case\n9\n of\n 1\n ->\n 10\n _\n ->\n 20"
        , testCase "(2)" $ assertFailure expr "case\n 9\nof\n 1\n ->\n 10\n _\n ->\n 20"
        , testCase "(3)" $ assertFailure expr "case\n 9\n of\n1\n ->\n 10\n _\n ->\n 20"
        , testCase "(4)" $ assertFailure expr "case\n 9\n of\n 1\n->\n 10\n _\n ->\n 20"
        , testCase "(5)" $ assertFailure expr "case\n 9\n of\n 1\n ->\n10\n _\n ->\n 20"
        , testCase "(6)" $ assertFailure expr "case\n 9\n of\n 1\n ->\n 10\n_\n ->\n 20"
        , testCase "(7)" $ assertFailure expr "case\n 9\n of\n 1\n ->\n 10\n _\n->\n 20"
        , testCase "(8)" $ assertFailure expr "case\n 9\n of\n 1\n ->\n 10\n _\n ->\n20"
        ]

    , testCase "tuple" $
        assertParse expr "(1,2)" $ at 1 1 1 6 $ Tuple [intExpr' (1,2,1,3) 1, intExpr' (1,4,1,5) 2] False
    , testCase "tuple (whitespace)" $
        assertParse expr "( 1 , 2 )" $ at 1 1 1 10 $ Tuple [intExpr' (1,3,1,4) 1, intExpr' (1,7,1,8) 2] False
    , testCase "tuple (comments)" $
        assertParse expr "({-A-}1{-B-},{-C-}2{-D-})" $ at 1 1 1 26 $ Tuple [commentedIntExpr (1,7,1,8) "A" "B" 1, commentedIntExpr (1,19,1,20) "C" "D" 2] False
    , testCase "tuple (newlines)" $
        assertParse expr "(\n 1\n ,\n 2\n )" $ at 1 1 5 3 $ Tuple [intExpr' (2,2,2,3) 1, intExpr' (4,2,4,3) 2] True
    , testGroup "tuple (must be indented)"
        [ testCase "(1)" $ assertFailure expr "(\n1\n ,\n 2\n )"
        , testCase "(2)" $ assertFailure expr "(\n 1\n,\n 2\n )"
        , testCase "(3)" $ assertFailure expr "(\n 1\n ,\n2\n )"
        , testCase "(4)" $ assertFailure expr "(\n 1\n ,\n 2\n)"
        ]

    , testCase "record (empty)" $
        assertParse expr "{}" $ at 1 1 1 3 $ Record [] False
    , testCase "record (empty, whitespace)" $
        assertParse expr "{ }" $ at 1 1 1 4 $ Record [] False
    , testCase "record" $
        assertParse expr "{x=7,y=8}" $ at 1 1 1 10 $ Record [("x", intExpr (1,4,1,5) 7, False), ("y", intExpr (1,8,1,9) 8, False)] False
    , testCase "record (single field)" $
        assertParse expr "{x=7}" $ at 1 1 1 6 $ Record [("x", intExpr (1,4,1,5) 7, False)] False
    , testCase "record (whitespace)" $
        assertParse expr "{ x = 7 , y = 8 }" $ at 1 1 1 18 $ Record [("x", intExpr (1,7,1,8) 7, False), ("y", intExpr (1,15,1,16) 8, False)] False
    , testCase "record (newlines)" $
        assertParse expr "{\n x\n =\n 7\n ,\n y\n =\n 8\n }" $ at 1 1 9 3 $ Record [("x", intExpr (4,2,4,3) 7, True), ("y", intExpr (8,2,8,3) 8, True)] True
    , testGroup "record (must be indented)"
        [ testCase "(1)" $ assertFailure expr "{\nx\n =\n 7\n ,\n y\n =\n 8\n }"
        , testCase "(2)" $ assertFailure expr "{\n x\n=\n 7\n ,\n y\n =\n 8\n }"
        , testCase "(3)" $ assertFailure expr "{\n x\n =\n7\n ,\n y\n =\n 8\n }"
        , testCase "(4)" $ assertFailure expr "{\n x\n =\n 7\n,\n y\n =\n 8\n }"
        , testCase "(5)" $ assertFailure expr "{\n x\n =\n 7\n ,\ny\n =\n 8\n }"
        , testCase "(6)" $ assertFailure expr "{\n x\n =\n 7\n ,\n y\n=\n 8\n }"
        , testCase "(7)" $ assertFailure expr "{\n x\n =\n 7\n ,\n y\n =\n8\n }"
        , testCase "(8)" $ assertFailure expr "{\n x\n =\n 7\n ,\n y\n =\n 8\n}"
        ]

    , testCase "record update" $
        assertParse expr "{a|x=7,y=8}" $ at 1 1 1 12 $ RecordUpdate (at 1 2 1 3 $ Var $ VarRef "a") [("x", intExpr (1,6,1,7) 7, False), ("y", intExpr (1,10,1,11) 8, False)] False
    , testCase "record update (single field)" $
        assertParse expr "{a|x=7}" $ at 1 1 1 8 $ RecordUpdate (at 1 2 1 3 $ Var $ VarRef "a") [("x", intExpr (1,6,1,7) 7, False)] False
    , testCase "record update (whitespace)" $
        assertParse expr "{ a | x = 7 , y = 8 }" $ at 1 1 1 22 $ RecordUpdate (at 1 3 1 4 $ Var $ VarRef "a") [("x", intExpr (1,11,1,12) 7, False), ("y", intExpr (1,19,1,20) 8, False)] False
    , testCase "record update (newlines)" $
        assertParse expr "{\n a\n |\n x\n =\n 7\n ,\n y\n =\n 8\n }" $ at 1 1 11 3 $ RecordUpdate (at 2 2 2 3 $ Var $ VarRef "a") [("x", intExpr (6,2,6,3) 7, True), ("y", intExpr (10,2,10,3) 8, True)] True
    , testCase "record update (only allows simple base)" $
        assertFailure expr "{9|x=7}"
    , testCase "record update (only allows simple base)" $
        assertFailure expr "{{}|x=7}"
    , testGroup "record update (must be indented)" $
        [ testCase "(1)" $ assertFailure expr "{\na\n |\n x\n =\n 7\n ,\n y\n =\n 8\n }"
        , testCase "(2)" $ assertFailure expr "{\n a\n|\n x\n =\n 7\n ,\n y\n =\n 8\n }"
        , testCase "(3)" $ assertFailure expr "{\n a\n |\nx\n =\n 7\n ,\n y\n =\n 8\n }"
        , testCase "(4)" $ assertFailure expr "{\n a\n |\n x\n=\n 7\n ,\n y\n =\n 8\n }"
        , testCase "(5)" $ assertFailure expr "{\n a\n |\n x\n =\n7\n ,\n y\n =\n 8\n }"
        , testCase "(6)" $ assertFailure expr "{\n a\n |\n x\n =\n 7\n,\n y\n =\n 8\n }"
        , testCase "(7)" $ assertFailure expr "{\n a\n |\n x\n =\n 7\n ,\ny\n =\n 8\n }"
        , testCase "(8)" $ assertFailure expr "{\n a\n |\n x\n =\n 7\n ,\n y\n=\n 8\n }"
        , testCase "(9)" $ assertFailure expr "{\n a\n |\n x\n =\n 7\n ,\n y\n =\n8\n }"
        , testCase "(10)" $ assertFailure expr "{\n a\n |\n x\n =\n 7\n ,\n y\n =\n 8\n}"
        ]

    , testCase "function application" $
        assertParse expr "f 7 8" $ at 1 1 1 6 $ App (at 1 1 1 2 $ Var $ VarRef "f") [intExpr' (1,3,1,4) 7, intExpr' (1,5,1,6) 8] False
    , testCase "function application (comments)" $
        assertParse expr "f{-A-}7{-B-}8" $ at 1 1 1 14 $ App (at 1 1 1 2 $ Var $ VarRef "f") [commentedIntExpr' (1,7,1,8) "A" 7, commentedIntExpr' (1,13,1,14) "B" 8] False
    , testCase "function application (newlines)" $
        assertParse expr "f\n 7\n 8" $ at 1 1 3 3 $ App (at 1 1 1 2 $ Var $ VarRef "f") [intExpr' (2,2,2,3) 7, intExpr' (3,2,3,3) 8] True
    , testCase "function application (newlines, comments)" $
        assertParse expr "f\n {-A-}7\n {-B-}8" $ at 1 1 3 8 $ App (at 1 1 1 2 $ Var $ VarRef "f") [commentedIntExpr' (2,7,2,8) "A" 7, commentedIntExpr' (3,7,3,8) "B" 8] True
    , testGroup "function application (must be indented)"
        [ testCase "(1)" $ assertFailure expr "f\n7\n 8"
        , testCase "(2)" $ assertFailure expr "f\n 7\n8"
        ]

    , testCase "parens" $
        assertParse expr "(1)" $ at 1 1 1 4 $ Parens (intExpr (1,2,1,3) 1) False
    , testCase "parens (whitespace)" $
        assertParse expr "( 1 )" $ at 1 1 1 6 $ Parens (intExpr (1,3,1,4) 1) False
    , testCase "parens (newlines)" $
        assertParse expr "(\n 1\n )" $ at 1 1 3 3 $ Parens (intExpr (2,2,2,3) 1) True
    , testGroup "parens (must be indented)"
        [ testCase "(1)" $ assertFailure expr "(\n1\n )"
        , testCase "(2)" $ assertFailure expr "(\n 1\n)"
        ]

    , testCase "unit" $
        assertParse expr "()" $ at 1 1 1 3 $ Unit


    , testGroup "definition"
        [ testCase "" $ assertParse definition "x=1" $ at 1 1 1 4 $ Definition (at 1 1 1 2 $ P.Var $ VarRef "x") [] [] (intExpr (1,3,1,4) 1) False
        , testCase "comments" $ assertParse definition "x{-A-}={-B-}1" $ at 1 1 1 14 $ Definition (at 1 1 1 2 $ P.Var $ VarRef "x") [] [BlockComment ["B"]] (intExpr (1,13,1,14) 1) False
        , testCase "line comments" $ assertParse definition "x =\n    --X\n    1" $ at 1 1 3 6 $ Definition (at 1 1 1 2 $ P.Var $ VarRef "x") [] [LineComment "X"] (intExpr (3,5,3,6) 1) True

        ]
    ]
