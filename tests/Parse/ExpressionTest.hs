module Parse.ExpressionTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Text.Lazy as LazyText

import Parse.Expression
import Parse.Helpers (IParser, iParse)
import AST.V0_16
import AST.Expression
import qualified AST.Pattern as P
import AST.Variable
import Reporting.Annotation hiding (map, at)
import Reporting.Region
import Text.Parsec.Char (string)

import Parse.TestHelpers


pending = at 0 0 0 0 $ Unit []


example name input expected =
    testCase name $
        assertParse expr input expected


commentedIntExpr (a,b,c,d) preComment postComment i =
    Commented [BlockComment [preComment]] (at a b c d  $ Literal $ IntNum i) [BlockComment [postComment]]

commentedIntExpr' (a,b,c,d) preComment i =
    Commented [BlockComment [preComment]] (at a b c d  $ Literal $ IntNum i) []


commentedIntExpr'' (a,b,c,d) preComment i =
    (,) [BlockComment [preComment]] $ at a b c d  $ Literal $ IntNum i


intExpr (a,b,c,d) i = at a b c d $ Literal $ IntNum i

intExpr' (a,b,c,d) i =
    Commented [] (at a b c d  $ Literal $ IntNum i) []

intExpr'' (a,b,c,d) i =
    (,) [] $ at a b c d  $ Literal $ IntNum i


tests :: Test
tests =
    testGroup "Parse.Expression"
    [ testGroup "Unit"
        [ example "" "()" $ at 1 1 1 3 $ Unit []
        , example "whitespace" "( )" $ at 1 1 1 4 $ Unit []
        , example "comments" "({-A-})" $ at 1 1 1 8 $ Unit [BlockComment ["A"]]
        , example "newlines" "(\n )" $ at 1 1 2 3 $ Unit []
        ]

    , testGroup "Literal"
        [ example "" "1" $ at 1 1 1 2 (Literal (IntNum 1))

        , testGroup "Boolean"
            [ example "True" "True" $ at 1 1 1 5 $ Literal $ Boolean True
            , example "False" "False" $ at 1 1 1 6 $ Literal $ Boolean False
            ]
        ]

    , testGroup "variable"
        [ example "lowercase" "foo" $ at 1 1 1 4 $ Var $ VarRef "foo"
        , example "uppercase" "Bar" $ at 1 1 1 4 $ Var $ VarRef "Bar"
        , example "qualified" "Bar.Baz.foo" $ at 1 1 1 12 $ Var $ VarRef "Bar.Baz.foo"

        , testGroup "symbolic operator"
            [ example "" "(+)" $ at 1 1 1 4 $ Var $ OpRef "+"
            , testCase "does not allow whitespace" $
                assertFailure expr "( + )"
            , testCase "doew not allow comments" $
                assertFailure expr "({-A-}+{-B-})"
            ]
        ]

    , testGroup "function application"
        [ example "" "f 7 8" $ at 1 1 1 6 $ App (at 1 1 1 2 $ Var $ VarRef "f") [intExpr'' (1,3,1,4) 7, intExpr'' (1,5,1,6) 8] False
        , example "argument starts with minus" "f -9 -x" $ at 1 1 1 8 $ App (at 1 1 1 2 $ Var $ VarRef "f") [intExpr'' (1,3,1,5) (-9), (,) [] $ at 1 6 1 8 $ Unary Negative $ at 1 7 1 8 $ Var $ VarRef "x"] False
        , example "comments" "f{-A-}7{-B-}8" $ at 1 1 1 14 $ App (at 1 1 1 2 $ Var $ VarRef "f") [commentedIntExpr'' (1,7,1,8) "A" 7, commentedIntExpr'' (1,13,1,14) "B" 8] False
        , example "newlines" "f\n 7\n 8" $ at 1 1 3 3 $ App (at 1 1 1 2 $ Var $ VarRef "f") [intExpr'' (2,2,2,3) 7, intExpr'' (3,2,3,3) 8] True
        , example "newlines and comments" "f\n {-A-}7\n {-B-}8" $ at 1 1 3 8 $ App (at 1 1 1 2 $ Var $ VarRef "f") [commentedIntExpr'' (2,7,2,8) "A" 7, commentedIntExpr'' (3,7,3,8) "B" 8] True
        ]

    , testGroup "unary operators"
        [ testGroup "negative"
            [ example "" "-True" $ at 1 1 1 6 $ Unary Negative $ at 1 2 1 6 $ Literal $ Boolean True
            , testCase "must not have whitespace" $
                assertFailure expr "- True"
            , testCase "must not have comment" $
                assertFailure expr "-{- -}True"
            , testCase "does not apply to '-'" $
                assertFailure expr "--True"
            , testCase "does not apply to '.'" $
                assertFailure expr "-.foo"
            ]
        ]

    , testGroup "binary operators"
        [ example "" "7+8<<>>9" $ at 1 1 1 9 $ Binops (intExpr (1,1,1,2) 7) [([], OpRef "+", [], intExpr (1,3,1,4) 8), ([], OpRef "<<>>", [], intExpr (1,8,1,9) 9)] False
        , example "minus with no whitespace" "9-1" $ at 1 1 1 4 $ Binops (intExpr (1,1,1,2) 9) [([], OpRef "-", [], intExpr (1,3,1,4) 1)] False
        , example "backticks" "7`plus`8`shift`9" $ at 1 1 1 17 $ Binops (intExpr (1,1,1,2) 7) [([], VarRef "plus", [], intExpr (1,8,1,9) 8), ([], VarRef "shift", [], intExpr (1,16,1,17) 9)] False
        , example "whitespace" "7 + 8 <<>> 9" $ at 1 1 1 13 $ Binops (intExpr (1,1,1,2) 7) [([], OpRef "+", [], intExpr (1,5,1,6) 8), ([], OpRef "<<>>", [], intExpr (1,12,1,13) 9)] False
        , example "comments" "7{-A-}+{-B-}8{-C-}<<>>{-D-}9" $ at 1 1 1 29 $ Binops (intExpr (1,1,1,2) 7) [([BlockComment ["A"]], OpRef "+", [BlockComment ["B"]], intExpr (1,13,1,14) 8), ([BlockComment ["C"]], OpRef "<<>>", [BlockComment ["D"]], intExpr (1,28,1,29) 9)] False
        , example "newlines" "7\n +\n 8\n <<>>\n 9" $ at 1 1 5 3 $ Binops (intExpr (1,1,1,2) 7) [([], OpRef "+", [], intExpr (3,2,3,3) 8), ([], OpRef "<<>>", [], intExpr (5,2,5,3) 9)] True
        ]

    , testGroup "parentheses"
        [ example "" "(1)" $ at 1 1 1 4 $ Parens $ intExpr' (1,2,1,3) 1
        , example "whitespace" "( 1 )" $ at 1 1 1 6 $ Parens $ intExpr' (1,3,1,4) 1
        , example "comments" "({-A-}1{-B-})" $ at 1 1 1 14 $ Parens $ commentedIntExpr (1,7,1,8) "A" "B" 1
        , example "newlines" "(\n 1\n )" $ at 1 1 3 3 $ Parens $ intExpr' (2,2,2,3) 1
        ]

    , testGroup "empty list"
        [ example "empty" "[]" $ at 1 1 1 3 (EmptyList [])
        , example "whitespace" "[ ]" $ at 1 1 1 4 (EmptyList [])
        , example "comments" "[{-A-}]" $ at 1 1 1 8 (EmptyList [BlockComment ["A"]])
        , example "newlines" "[\n ]" $ at 1 1 2 3 (EmptyList [])
        ]

    , testGroup "List"
        [ example "" "[1,2,3]" $ at 1 1 1 8 $ ExplicitList [intExpr' (1,2,1,3) 1, intExpr' (1,4,1,5) 2, intExpr' (1,6,1,7) 3] False
        , example "single element" "[1]" $ at 1 1 1 4 $ ExplicitList [intExpr' (1,2,1,3) 1] False
        , example "whitespace" "[ 1 , 2 , 3 ]" $ at 1 1 1 14 $ ExplicitList [intExpr' (1,3,1,4) 1, intExpr' (1,7,1,8) 2, intExpr' (1,11,1,12) 3] False
        , example "comments" "[{-A-}1{-B-},{-C-}2{-D-},{-E-}3{-F-}]" $ at 1 1 1 38 $ ExplicitList [commentedIntExpr (1,7,1,8) "A" "B" 1, commentedIntExpr (1,19,1,20) "C" "D" 2, commentedIntExpr (1,31,1,32) "E" "F" 3] False
        , example "newlines" "[\n 1\n ,\n 2\n ,\n 3\n ]" $ at 1 1 7 3 $ ExplicitList [intExpr' (2,2,2,3) 1, intExpr' (4,2,4,3) 2, intExpr' (6,2,6,3) 3] True
        ]

    , testGroup "Range"
        [ example "" "[7..9]" $ at 1 1 1 7 $ Range (intExpr' (1,2,1,3) 7) (intExpr' (1,5,1,6) 9) False
        , example "whitespace" "[ 7 .. 9 ]" $ at 1 1 1 11 $ Range (intExpr' (1,3,1,4) 7) (intExpr' (1,8,1,9) 9) False
        , example "comments" "[{-A-}7{-B-}..{-C-}9{-D-}]" $ at 1 1 1 27 $ Range (commentedIntExpr (1,7,1,8) "A" "B" 7) (commentedIntExpr (1,20,1,21) "C" "D" 9) False
        , example "newlines" "[\n 7\n ..\n 9\n ]" $ at 1 1 5 3 $ Range (intExpr' (2,2,2,3) 7) (intExpr' (4,2,4,3) 9) True
        ]

    , testGroup "Tuple"
        [ example "" "(1,2)" $ at 1 1 1 6 $ Tuple [intExpr' (1,2,1,3) 1, intExpr' (1,4,1,5) 2] False
        , example "whitespace" "( 1 , 2 )" $ at 1 1 1 10 $ Tuple [intExpr' (1,3,1,4) 1, intExpr' (1,7,1,8) 2] False
        , example "comments" "({-A-}1{-B-},{-C-}2{-D-})" $ at 1 1 1 26 $ Tuple [commentedIntExpr (1,7,1,8) "A" "B" 1, commentedIntExpr (1,19,1,20) "C" "D" 2] False
        , example "newlines" "(\n 1\n ,\n 2\n )" $ at 1 1 5 3 $ Tuple [intExpr' (2,2,2,3) 1, intExpr' (4,2,4,3) 2] True
        ]

    , testGroup "tuple constructor"
        [ example "" "(,,)" $ at 1 1 1 5 $ TupleFunction 3
        , testCase "does not allow whitespace (1)" $ assertFailure expr "( ,,)"
        , testCase "does not allow whitespace (2)" $ assertFailure expr "(, ,)"
        , testCase "does not allow whitespace (3)" $ assertFailure expr "(,, )"
        , testCase "does not allow comments (1)" $ assertFailure expr "({-A-},,)"
        , testCase "does not allow comments (2)" $ assertFailure expr "(,{-A-},)"
        , testCase "does not allow comments (3)" $ assertFailure expr "(,,{-A-})"
        ]

    , testGroup "Record"
        [ testGroup "empty"
            [ example "" "{}" $ at 1 1 1 3 $ EmptyRecord []
            , example "whitespace" "{ }" $ at 1 1 1 4 $ EmptyRecord []
            , example "comments" "{{-A-}}" $ at 1 1 1 8 $ EmptyRecord [BlockComment ["A"]]
            ]

        , example "" "{x=7,y=8}" $ at 1 1 1 10 $ Record [(Commented [] "x" [], intExpr' (1,4,1,5) 7, False), (Commented [] "y" [], intExpr' (1,8,1,9) 8, False)] False
        , example "single field" "{x=7}" $ at 1 1 1 6 $ Record [(Commented [] "x" [], intExpr' (1,4,1,5) 7, False)] False
        , example "whitespace" "{ x = 7 , y = 8 }" $ at 1 1 1 18 $ Record [(Commented [] "x" [], intExpr' (1,7,1,8) 7, False), (Commented [] "y" [], intExpr' (1,15,1,16) 8, False)] False
        , example "comments" "{{-A-}x{-B-}={-C-}7{-D-},{-E-}y{-F-}={-G-}8{-H-}}" $ at 1 1 1 50 $ Record [(Commented [BlockComment ["A"]] "x" [BlockComment ["B"]], commentedIntExpr (1,19,1,20) "C" "D" 7, False), (Commented [BlockComment ["E"]] "y" [BlockComment ["F"]], commentedIntExpr (1,43,1,44) "G" "H" 8, False)] False
        , example "single field with comments" "{{-A-}x{-B-}={-C-}7{-D-}}" $ at 1 1 1 26 $ Record [(Commented [BlockComment ["A"]] "x" [BlockComment ["B"]], commentedIntExpr (1,19,1,20) "C" "D" 7, False)] False
        , example "newlines" "{\n x\n =\n 7\n ,\n y\n =\n 8\n }" $ at 1 1 9 3 $ Record [(Commented [] "x" [], intExpr' (4,2,4,3) 7, True), (Commented [] "y" [], intExpr' (8,2,8,3) 8, True)] True
        ]

    , testGroup "Record update"
        [ example "" "{a|x=7,y=8}" $ at 1 1 1 12 (RecordUpdate (Commented [] (at 1 2 1 3 (Var (VarRef "a"))) []) [(Commented [] "x" [],Commented [] (at 1 6 1 7 (Literal (IntNum 7))) [],False),(Commented [] "y" [], Commented [] (at 1 10 1 11 (Literal (IntNum 8))) [],False)] False)
        , example "single field" "{a|x=7}" $ at 1 1 1 8 (RecordUpdate (Commented [] (at 1 2 1 3 (Var (VarRef "a"))) []) [(Commented [] "x" [], Commented [] (at 1 6 1 7 (Literal (IntNum 7))) [],False)] False)
        , example "whitespace" "{ a | x = 7 , y = 8 }" $ at 1 1 1 22 (RecordUpdate (Commented [] (at 1 3 1 4 (Var (VarRef "a"))) []) [(Commented [] "x" [], Commented [] (at 1 11 1 12 (Literal (IntNum 7))) [],False),(Commented [] "y" [], Commented [] (at 1 19 1 20 (Literal (IntNum 8))) [],False)] False)
        , example "comments" "{{-A-}a{-B-}|{-C-}x{-D-}={-E-}7{-F-},{-G-}y{-H-}={-I-}8{-J-}}" $ at 1 1 1 62 (RecordUpdate (Commented [BlockComment ["A"]] (at 1 7 1 8 (Var (VarRef "a"))) [BlockComment ["B"]]) [(Commented [BlockComment ["C"]] "x" [BlockComment ["D"]],Commented [BlockComment ["E"]] (at 1 31 1 32 (Literal (IntNum 7))) [BlockComment ["F"]],False),(Commented [BlockComment ["G"]] "y" [BlockComment ["H"]],Commented [BlockComment ["I"]] (at 1 55 1 56 (Literal (IntNum 8))) [BlockComment ["J"]],False)] False)
        , example "newlines" "{\n a\n |\n x\n =\n 7\n ,\n y\n =\n 8\n }" $ at 1 1 11 3 (RecordUpdate (Commented [] (at 2 2 2 3 (Var (VarRef "a"))) []) [(Commented [] "x" [], Commented [] (at 6 2 6 3 (Literal (IntNum 7))) [],True),(Commented [] "y" [], Commented [] (at 10 2 10 3 (Literal (IntNum 8))) [],True)] True)
        , testCase "only allows simple base" $
            assertFailure expr "{9|x=7}"
        , testCase "only allows simple base" $
            assertFailure expr "{{}|x=7}"
        , testCase "must have fields" $
            assertFailure expr "{a|}"
        ]

    , testGroup "record access"
        [ example "" "x.f1" $ at 1 1 1 5 (Access (at 1 1 1 2 (Var (VarRef "x"))) "f1")
        , example "nested" "x.f1.f2" $ at 1 1 1 8 (Access (at 1 1 1 5 (Access (at 1 1 1 2 (Var (VarRef "x"))) "f1")) "f2")
        , testCase "does not allow symbolic field names" $
            assertFailure expr "x.+"
        , testCase "does not allow symbolic field names" $
            assertFailure expr "x.(+)"
        ]

    , testGroup "record access fuction"
        [ example "" ".f1" $ at 1 1 1 4 $ AccessFunction "f1"
        ]

    , testGroup "lambda"
        [ example "" "\\x y->9" $ at 1 1 1 8 $ Lambda [([], at 1 2 1 3 $ P.Var $ VarRef "x"), ([], at 1 4 1 5 $ P.Var $ VarRef "y")] [] (intExpr (1,7,1,8) 9) False
        , example "single parameter" "\\x->9" $ at 1 1 1 6 $ Lambda [([], at 1 2 1 3 $ P.Var $ VarRef "x")] [] (intExpr (1,5,1,6) 9) False
        , example "whitespace" "\\ x y -> 9" $ at 1 1 1 11 $ Lambda [([], at 1 3 1 4 $ P.Var $ VarRef "x"), ([], at 1 5 1 6 $ P.Var $ VarRef "y")] [] (intExpr (1,10,1,11) 9) False
        , example "comments" "\\{-A-}x{-B-}y{-C-}->{-D-}9" $ at 1 1 1 27 $ Lambda [([BlockComment ["A"]], at 1 7 1 8 $ P.Var $ VarRef "x"), ([BlockComment ["B"]], at 1 13 1 14 $ P.Var $ VarRef "y")] [BlockComment ["C"], BlockComment ["D"]] (intExpr (1,26,1,27) 9) False
        , example "newlines" "\\\n x\n y\n ->\n 9" $ at 1 1 5 3 $ Lambda [([], at 2 2 2 3 $ P.Var $ VarRef "x"), ([], at 3 2 3 3 $ P.Var $ VarRef "y")] [] (intExpr (5,2,5,3) 9) True
        , testCase "arrow must not contain whitespace" $
            assertFailure expr "\\x y - > 9"
        ]

    , testGroup "if statement"
        [ example "" "if x then y else z" $ at 1 1 1 19 (If (Commented [] (at 1 4 1 5 (Var (VarRef "x"))) [],Commented [] (at 1 11 1 12 (Var (VarRef "y"))) []) [] ([],at 1 18 1 19 (Var (VarRef "z"))))
        , example "comments" "if{-A-}x{-B-}then{-C-}y{-D-}else{-E-}if{-F-}x'{-G-}then{-H-}y'{-I-}else{-J-}z" $ at 1 1 1 78 (If (Commented [BlockComment ["A"]] (at 1 8 1 9 (Var (VarRef "x"))) [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 23 1 24 (Var (VarRef "y"))) [BlockComment ["D"]]) [([BlockComment ["E"]],(Commented [BlockComment ["F"]] (at 1 45 1 47 (Var (VarRef "x'"))) [BlockComment ["G"]],Commented [BlockComment ["H"]] (at 1 61 1 63 (Var (VarRef "y'"))) [BlockComment ["I"]]))] ([BlockComment ["J"]],at 1 77 1 78 (Var (VarRef "z"))))
        , example "else if" "if x then y else if x' then y' else if x'' then y'' else z" $ at 1 1 1 59 (If (Commented [] (at 1 4 1 5 (Var (VarRef "x"))) [],Commented [] (at 1 11 1 12 (Var (VarRef "y"))) []) [([],(Commented [] (at 1 21 1 23 (Var (VarRef "x'"))) [],Commented [] (at 1 29 1 31 (Var (VarRef "y'"))) [])),([],(Commented [] (at 1 40 1 43 (Var (VarRef "x''"))) [],Commented [] (at 1 49 1 52 (Var (VarRef "y''"))) []))] ([],at 1 58 1 59 (Var (VarRef "z"))))
        , example "newlines" "if\n x\n then\n y\n else\n z" $ at 1 1 6 3 (If (Commented [] (at 2 2 2 3 (Var (VarRef "x"))) [],Commented [] (at 4 2 4 3 (Var (VarRef "y"))) []) [] ([],at 6 2 6 3 (Var (VarRef "z"))))
        ]

    , testGroup "let statement"
        [ example "" "let a=b in z" $ at 1 1 1 13 (Let [at 1 5 1 8 (Definition (at 1 5 1 6 (P.Var (VarRef "a"))) [] [] (at 1 7 1 8 (Var (VarRef "b"))) False)] [] (at 1 12 1 13 (Var (VarRef "z"))))
        , example "multiple declarations" "let a=b\n    c=d\nin z" $ at 1 1 3 5 (Let [at 1 5 1 8 (Definition (at 1 5 1 6 (P.Var (VarRef "a"))) [] [] (at 1 7 1 8 (Var (VarRef "b"))) False),at 2 5 2 8 (Definition (at 2 5 2 6 (P.Var (VarRef "c"))) [] [] (at 2 7 2 8 (Var (VarRef "d"))) False)] [] (at 3 4 3 5 (Var (VarRef "z"))))
        , example "multiple declarations" "let\n a=b\n c=d\nin z" $ at 1 1 4 5 (Let [at 2 2 2 5 (Definition (at 2 2 2 3 (P.Var (VarRef "a"))) [] [] (at 2 4 2 5 (Var (VarRef "b"))) False),at 3 2 3 5 (Definition (at 3 2 3 3 (P.Var (VarRef "c"))) [] [] (at 3 4 3 5 (Var (VarRef "d"))) False)] [] (at 4 4 4 5 (Var (VarRef "z"))))
        , example "whitespace" "let a = b in z" $ at 1 1 1 15 (Let [at 1 5 1 10 (Definition (at 1 5 1 6 (P.Var (VarRef "a"))) [] [] (at 1 9 1 10 (Var (VarRef "b"))) False)] [] (at 1 14 1 15 (Var (VarRef "z"))))
        , example "comments" "let{-A-}a{-B-}={-C-}b{-D-}in{-E-}z" $ at 1 1 1 35 (Let [at 0 0 0 0 (LetComment (BlockComment ["A"])),at 1 9 1 22 (Definition (at 1 9 1 10 (P.Var (VarRef "a"))) [] [BlockComment ["B"],BlockComment ["C"]] (at 1 21 1 22 (Var (VarRef "b"))) False),at 0 0 0 0 (LetComment (BlockComment ["D"]))] [BlockComment ["E"]] (at 1 34 1 35 (Var (VarRef "z"))))
        , example "newlines" "let\n a\n =\n b\nin\n z" $ at 1 1 6 3 (Let [at 2 2 4 3 (Definition (at 2 2 2 3 (P.Var (VarRef "a"))) [] [] (at 4 2 4 3 (Var (VarRef "b"))) True)] [] (at 6 2 6 3 (Var (VarRef "z"))))
        , testCase "must have at least one definition" $
            assertFailure expr "let in z"
        , testGroup "declarations must start at the same column" $
            [ testCase "(1)" $ assertFailure expr "let a=b\n   c=d\nin z"
            , testCase "(2)" $ assertFailure expr "let a=b\n     c=d\nin z"
            , testCase "(3)" $ assertFailure expr "let  a=b\n   c=d\nin z"
            ]
        ]

    , testGroup "case statement"
        [ example "" "case 9 of\n 1->10\n _->20" $ at 1 1 3 7 $ Case (intExpr (1,6,1,7) 9, False) [([], at 2 2 2 3 $ P.Literal $ IntNum 1, [], intExpr (2,5,2,7) 10), ([], at 3 2 3 3 $ P.Anything, [], intExpr (3,5,3,7) 20)]
        , example "no newline after 'of'" "case 9 of 1->10\n          _->20" $ at 1 1 2 16 $ Case (intExpr (1,6,1,7) 9, False) [([], at 1 11 1 12 $ P.Literal $ IntNum 1, [], intExpr (1,14,1,16) 10), ([], at 2 11 2 12 $ P.Anything, [], intExpr (2,14,2,16) 20)]
        , example "whitespace" "case 9 of\n 1 -> 10\n _ -> 20" $ at 1 1 3 9 $ Case (intExpr (1,6,1,7) 9, False) [([], at 2 2 2 3 $ P.Literal $ IntNum 1, [], intExpr (2,7,2,9) 10), ([], at 3 2 3 3 $ P.Anything, [], intExpr (3,7,3,9) 20)]
         -- TODO: handle comments A, B, E, I, and don't allow K
        , example "comments" "case{-A-}9{-B-}of{-C-}\n{-D-}1{-E-}->{-F-}10{-G-}\n{-H-}_{-I-}->{-J-}20" $ at 1 1 3 21 $ Case (intExpr (1,10,1,11) 9, False) [([BlockComment ["C"], BlockComment ["D"]], at 2 6 2 7 $ P.Literal $ IntNum 1, [BlockComment ["F"]], intExpr (2,19,2,21) 10), ([BlockComment ["G"], BlockComment ["H"]], at 3 6 3 7 $ P.Anything, [BlockComment ["J"]], intExpr (3,19,3,21) 20)]
        , example "newlines" "case\n 9\n of\n 1\n ->\n 10\n _\n ->\n 20" $ at 1 1 9 4 $ Case (intExpr (2,2,2,3) 9, True) [([], at 4 2 4 3 $ P.Literal $ IntNum 1, [], intExpr (6,2,6,4) 10), ([], at 7 2 7 3 $ P.Anything, [], intExpr (9,2,9,4) 20)]
        , testCase "should not consume trailing whitespace" $
            assertParse (expr >> string "\nX") "case 9 of\n 1->10\n _->20\nX" $ "\nX"
        , testGroup "clauses must start at the same column"
            [ testCase "(1)" $ assertFailure expr "case 9 of\n 1->10\n_->20"
            , testCase "(2)" $ assertFailure expr "case 9 of\n 1->10\n  _->20"
            , testCase "(3)" $ assertFailure expr "case 9 of\n  1->10\n _->20"
            ]
        ]

    , testGroup "definition"
        [ testCase "" $ assertParse definition "x=1" $ at 1 1 1 4 $ Definition (at 1 1 1 2 $ P.Var $ VarRef "x") [] [] (intExpr (1,3,1,4) 1) False
        , testCase "comments" $ assertParse definition "x{-A-}={-B-}1" $ at 1 1 1 14 $ Definition (at 1 1 1 2 $ P.Var $ VarRef "x") [] [BlockComment ["A"], BlockComment ["B"]] (intExpr (1,13,1,14) 1) False
        , testCase "line comments" $ assertParse definition "x\n--Y\n =\n    --X\n    1" $ at 1 1 5 6 $ Definition (at 1 1 1 2 $ P.Var $ VarRef "x") [] [LineComment "Y", LineComment "X"] (intExpr (5,5,5,6) 1) True
        ]
    ]
