module Parse.ExpressionTest where

import Test.Tasty
import Test.Tasty.HUnit

import Parse.Expression
import AST.V0_16
import AST.Expression
import AST.Pattern (Pattern'(Anything))
import qualified AST.Pattern as P
import AST.Variable
import qualified Box
import qualified Data.Bimap as Bimap
import Data.Fix
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import Text.Parsec.Char (string)
import ElmFormat.ImportInfo (ImportInfo(..))
import ElmFormat.Render.Box (formatExpression, ExpressionContext(..))
import ElmVersion
import Parse.TestHelpers


pending :: Expr
pending = Fix $ AE $ at 0 0 0 0 $ Unit []


example :: String -> String -> Expr -> TestTree
example name input expected =
    testCase name $
        assertParse (expr Elm_0_19) input expected


importInfo :: ImportInfo
importInfo =
    ImportInfo Map.empty Bimap.empty


example' :: String -> String -> String -> TestTree
example' name input expected =
    testCase name $
        assertParse (fmap (Text.unpack . Box.render . formatExpression Elm_0_19 importInfo SyntaxSeparated . stripAnnotation) (expr Elm_0_19)) input expected


commentedIntExpr (a,b,c,d) preComment postComment i =
    Commented [BlockComment [preComment]] (Fix $ AE $ at a b c d  $ Literal $ IntNum i DecimalInt) [BlockComment [postComment]]

commentedIntExpr' (a,b,c,d) preComment i =
    Commented [BlockComment [preComment]] (Fix $ AE $ at a b c d  $ Literal $ IntNum i DecimalInt) []


commentedIntExpr'' (a,b,c,d) preComment i =
    (,) [BlockComment [preComment]] $ Fix $ AE $ at a b c d  $ Literal $ IntNum i DecimalInt


intExpr (a,b,c,d) i = Fix $ AE $ at a b c d $ Literal $ IntNum i DecimalInt

intExpr' (a,b,c,d) i =
    Commented [] (Fix $ AE $ at a b c d  $ Literal $ IntNum i DecimalInt) []

intExpr'' (a,b,c,d) i =
    (,) [] $ Fix $ AE $ at a b c d  $ Literal $ IntNum i DecimalInt


tests :: TestTree
tests =
    testGroup "Parse.Expression"
    [ testGroup "Unit"
        [ example "" "()" $ Fix $ AE $ at 1 1 1 3 $ Unit []
        , example "whitespace" "( )" $ Fix $ AE $ at 1 1 1 4 $ Unit []
        , example "comments" "({-A-})" $ Fix $ AE $ at 1 1 1 8 $ Unit [BlockComment ["A"]]
        , example "newlines" "(\n )" $ Fix $ AE $ at 1 1 2 3 $ Unit []
        ]

    , testGroup "Literal"
        [ example "" "1" $ Fix $ AE $ at 1 1 1 2 (Literal (IntNum 1 DecimalInt))

        , testGroup "Boolean"
            [ example "True" "True" $ Fix $ AE $ at 1 1 1 5 $ Literal $ Boolean True
            , example "False" "False" $ Fix $ AE $ at 1 1 1 6 $ Literal $ Boolean False
            ]
        ]

    , testGroup "variable"
        [ example "lowercase" "foo" $ Fix $ AE $ at 1 1 1 4 $ VarExpr $ VarRef [] $ LowercaseIdentifier "foo"
        , example "uppercase" "Bar" $ Fix $ AE $ at 1 1 1 4 $ VarExpr $ TagRef [] $ UppercaseIdentifier "Bar"
        , example "qualified" "Bar.Baz.foo" $ Fix $ AE $ at 1 1 1 12 $ VarExpr $ VarRef [UppercaseIdentifier "Bar", UppercaseIdentifier "Baz"] $ LowercaseIdentifier "foo"

        , testGroup "symbolic operator"
            [ example "" "(+)" $ Fix $ AE $ at 1 1 1 4 $ VarExpr $ (OpRef $ SymbolIdentifier "+")
            , testCase "does not allow whitespace" $
                assertParseFailure (expr Elm_0_19) "( + )"
            , testCase "doew not allow comments" $
                assertParseFailure (expr Elm_0_19) "({-A-}+{-B-})"
            ]
        ]

    , testGroup "function application"
        [ example "" "f 7 8" $ Fix $ AE $ at 1 1 1 6 $ App (Fix $ AE $ at 1 1 1 2 $ VarExpr $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (1,3,1,4) 7, intExpr'' (1,5,1,6) 8] (FAJoinFirst JoinAll)
        , example "argument starts with minus" "f -9 -x" $ Fix $ AE $ at 1 1 1 8 $ App (Fix $ AE $ at 1 1 1 2 $ VarExpr $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (1,3,1,5) (-9), (,) [] $ Fix $ AE $ at 1 6 1 8 $ Unary Negative $ Fix $ AE $ at 1 7 1 8 $ VarExpr $ VarRef [] $ LowercaseIdentifier "x"] (FAJoinFirst JoinAll)
        , example "comments" "f{-A-}7{-B-}8" $ Fix $ AE $ at 1 1 1 14 $ App (Fix $ AE $ at 1 1 1 2 $ VarExpr $ VarRef [] $ LowercaseIdentifier "f") [commentedIntExpr'' (1,7,1,8) "A" 7, commentedIntExpr'' (1,13,1,14) "B" 8] (FAJoinFirst JoinAll)
        , example "newlines (1)" "f 7\n 8" $ Fix $ AE $ at 1 1 2 3 $ App (Fix $ AE $ at 1 1 1 2 $ VarExpr $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (1,3,1,4) 7, intExpr'' (2,2,2,3) 8] (FAJoinFirst SplitAll)
        , example "newlines (2)" "f\n 7\n 8" $ Fix $ AE $ at 1 1 3 3 $ App (Fix $ AE $ at 1 1 1 2 $ VarExpr $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (2,2,2,3) 7, intExpr'' (3,2,3,3) 8] FASplitFirst
        , example "newlines and comments" "f\n {-A-}7\n {-B-}8" $ Fix $ AE $ at 1 1 3 8 $ App (Fix $ AE $ at 1 1 1 2 $ VarExpr $ VarRef [] $ LowercaseIdentifier "f") [commentedIntExpr'' (2,7,2,8) "A" 7, commentedIntExpr'' (3,7,3,8) "B" 8] FASplitFirst
        ]

    , testGroup "unary operators"
        [ testGroup "negative"
            [ example "" "-True" $ Fix $ AE $ at 1 1 1 6 $ Unary Negative $ Fix $ AE $ at 1 2 1 6 $ Literal $ Boolean True
            , testCase "must not have whitespace" $
                assertParseFailure (expr Elm_0_19) "- True"
            , testCase "must not have comment" $
                assertParseFailure (expr Elm_0_19) "-{- -}True"
            , testCase "does not apply to '-'" $
                assertParseFailure (expr Elm_0_19) "--True"
            , testCase "does not apply to '.'" $
                assertParseFailure (expr Elm_0_19) "-.foo"
            ]
        ]

    , testGroup "binary operators"
        [ example "" "7+8<<>>9" $ Fix $ AE $ at 1 1 1 9 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (OpRef $ SymbolIdentifier "+") [] (intExpr (1,3,1,4) 8), BinopsClause [] (OpRef $ SymbolIdentifier "<<>>") [] (intExpr (1,8,1,9) 9)] False
        , example "minus with no whitespace" "9-1" $ Fix $ AE $ at 1 1 1 4 $ Binops (intExpr (1,1,1,2) 9) [BinopsClause [] (OpRef $ SymbolIdentifier "-") [] (intExpr (1,3,1,4) 1)] False
        , example "backticks" "7`plus`8`shift`9" $ Fix $ AE $ at 1 1 1 17 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (VarRef [] $ LowercaseIdentifier "plus") [] (intExpr (1,8,1,9) 8), BinopsClause [] (VarRef [] $ LowercaseIdentifier "shift") [] (intExpr (1,16,1,17) 9)] False
        , example "whitespace" "7 + 8 <<>> 9" $ Fix $ AE $ at 1 1 1 13 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (OpRef $ SymbolIdentifier "+") [] (intExpr (1,5,1,6) 8), BinopsClause [] (OpRef $ SymbolIdentifier "<<>>") [] (intExpr (1,12,1,13) 9)] False
        , example "comments" "7{-A-}+{-B-}8{-C-}<<>>{-D-}9" $ Fix $ AE $ at 1 1 1 29 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [BlockComment ["A"]] (OpRef $ SymbolIdentifier "+") [BlockComment ["B"]] (intExpr (1,13,1,14) 8), BinopsClause [BlockComment ["C"]] (OpRef $ SymbolIdentifier "<<>>") [BlockComment ["D"]] (intExpr (1,28,1,29) 9)] False
        , example "newlines" "7\n +\n 8\n <<>>\n 9" $ Fix $ AE $ at 1 1 5 3 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (OpRef $ SymbolIdentifier "+") [] (intExpr (3,2,3,3) 8), BinopsClause [] (OpRef $ SymbolIdentifier "<<>>") [] (intExpr (5,2,5,3) 9)] True
        ]

    , testGroup "parentheses"
        [ example "" "(1)" $ Fix $ AE $ at 1 1 1 4 $ Parens $ intExpr' (1,2,1,3) 1
        , example "whitespace" "( 1 )" $ Fix $ AE $ at 1 1 1 6 $ Parens $ intExpr' (1,3,1,4) 1
        , example "comments" "({-A-}1{-B-})" $ Fix $ AE $ at 1 1 1 14 $ Parens $ commentedIntExpr (1,7,1,8) "A" "B" 1
        , example "newlines" "(\n 1\n )" $ Fix $ AE $ at 1 1 3 3 $ Parens $ intExpr' (2,2,2,3) 1
        ]

    , testGroup "empty list"
        [ example' "empty" "[]" "[]\n"
        , example' "whitespace" "[ ]" "[]\n"
        , example' "comments" "[{-A-}]" "[{- A -}]\n"
        , example' "newlines" "[\n ]" "[]\n"
        ]

    , testGroup "List"
        [ example' "" "[1,2,3]" "[ 1, 2, 3 ]\n"
        , example' "single element" "[1]" "[ 1 ]\n"
        , example' "whitespace" "[ 1 , 2 , 3 ]" "[ 1, 2, 3 ]\n"
        , example' "comments"
            "[{-A-}1{-B-},{-C-}2{-D-},{-E-}3{-F-}]"
            "[ {- A -} 1\n\
            \\n\
            \{- B -}\n\
            \, {- C -} 2\n\
            \\n\
            \{- D -}\n\
            \, {- E -} 3\n\
            \\n\
            \{- F -}\n\
            \]\n"
        , example' "newlines"
            "[\n 1\n ,\n 2\n ,\n 3\n ]"
            "[ 1\n, 2\n, 3\n]\n"
        ]

    , testGroup "Range"
        [ example "" "[7..9]" $ Fix $ AE $ at 1 1 1 7 $ Range (intExpr' (1,2,1,3) 7) (intExpr' (1,5,1,6) 9) False
        , example "whitespace" "[ 7 .. 9 ]" $ Fix $ AE $ at 1 1 1 11 $ Range (intExpr' (1,3,1,4) 7) (intExpr' (1,8,1,9) 9) False
        , example "comments" "[{-A-}7{-B-}..{-C-}9{-D-}]" $ Fix $ AE $ at 1 1 1 27 $ Range (commentedIntExpr (1,7,1,8) "A" "B" 7) (commentedIntExpr (1,20,1,21) "C" "D" 9) False
        , example "newlines" "[\n 7\n ..\n 9\n ]" $ Fix $ AE $ at 1 1 5 3 $ Range (intExpr' (2,2,2,3) 7) (intExpr' (4,2,4,3) 9) True
        ]

    , testGroup "Tuple"
        [ example "" "(1,2)" $ Fix $ AE $ at 1 1 1 6 $ Tuple [intExpr' (1,2,1,3) 1, intExpr' (1,4,1,5) 2] False
        , example "whitespace" "( 1 , 2 )" $ Fix $ AE $ at 1 1 1 10 $ Tuple [intExpr' (1,3,1,4) 1, intExpr' (1,7,1,8) 2] False
        , example "comments" "({-A-}1{-B-},{-C-}2{-D-})" $ Fix $ AE $ at 1 1 1 26 $ Tuple [commentedIntExpr (1,7,1,8) "A" "B" 1, commentedIntExpr (1,19,1,20) "C" "D" 2] False
        , example "newlines" "(\n 1\n ,\n 2\n )" $ Fix $ AE $ at 1 1 5 3 $ Tuple [intExpr' (2,2,2,3) 1, intExpr' (4,2,4,3) 2] True
        ]

    , testGroup "tuple constructor"
        [ example "" "(,,)" $ Fix $ AE $ at 1 1 1 5 $ TupleFunction 3
        , testCase "does not allow whitespace (1)" $ assertParseFailure (expr Elm_0_19) "( ,,)"
        , testCase "does not allow whitespace (2)" $ assertParseFailure (expr Elm_0_19) "(, ,)"
        , testCase "does not allow whitespace (3)" $ assertParseFailure (expr Elm_0_19) "(,, )"
        , testCase "does not allow comments (1)" $ assertParseFailure (expr Elm_0_19) "({-A-},,)"
        , testCase "does not allow comments (2)" $ assertParseFailure (expr Elm_0_19) "(,{-A-},)"
        , testCase "does not allow comments (3)" $ assertParseFailure (expr Elm_0_19) "(,,{-A-})"
        ]

    , testGroup "Record"
        [ testGroup "empty"
            [ example' "" "{}" "{}\n"
            , example' "whitespace" "{ }" "{}\n"
            , example' "comments" "{{-A-}}" "{{- A -}}\n"
            ]

        , example' ""
            "{x=7,y=8}"
            "{ x = 7, y = 8 }\n"
        , example' "single field"
            "{x=7}"
            "{ x = 7 }\n"
        , example' "whitespace"
            "{ x = 7 , y = 8 }"
            "{ x = 7, y = 8 }\n"
        , example' "comments"
            "{{-A-}x{-B-}={-C-}7{-D-},{-E-}y{-F-}={-G-}8{-H-}}"
            "{ {- A -} x {- B -} = {- C -} 7\n\n{- D -}\n, {- E -} y {- F -} = {- G -} 8\n\n{- H -}\n}\n"
        , example' "single field with comments"
            "{{-A-}x{-B-}={-C-}7{-D-}}"
            "{ {- A -} x {- B -} = {- C -} 7\n\n{- D -}\n}\n"
        , example' "newlines"
            "{\n x\n =\n 7\n ,\n y\n =\n 8\n }"
            "{ x =\n    7\n, y =\n    8\n}\n"
        ]

    , testGroup "Record update"
        [ example' ""
            "{a|x=7,y=8}"
            "{ a | x = 7, y = 8 }\n"
        , example' "single field"
            "{a|x=7}"
            "{ a | x = 7 }\n"
        , example' "whitespace"
            "{ a | x = 7 , y = 8 }"
            "{ a | x = 7, y = 8 }\n"
        , example' "comments"
            "{{-A-}a{-B-}|{-C-}x{-D-}={-E-}7{-F-},{-G-}y{-H-}={-I-}8{-J-}}"
            "{ {- A -} a {- B -}\n    | {- C -} x {- D -} = {- E -} 7\n\n    {- F -}\n    , {- G -} y {- H -} = {- I -} 8\n\n    {- J -}\n}\n"
        , example' "newlines"
            "{\n a\n |\n x\n =\n 7\n ,\n y\n =\n 8\n }"
            "{ a\n    | x =\n        7\n    , y =\n        8\n}\n"
        , testCase "only allows simple base" $
            assertParseFailure (expr Elm_0_19) "{9|x=7}"
        , testCase "only allows simple base" $
            assertParseFailure (expr Elm_0_19) "{{}|x=7}"
        , example' "no fields (elm-compiler does not allow this)"
            "{a|}"
            "{ a |  }\n"
        ]

    , testGroup "record access"
        [ example "" "x.f1" $ Fix $ AE $ at 1 1 1 5 (Access (Fix $ AE $ at 1 1 1 2 (VarExpr (VarRef [] $ LowercaseIdentifier "x"))) (LowercaseIdentifier "f1"))
        , example "nested" "x.f1.f2" $ Fix $ AE $ at 1 1 1 8 (Access (Fix $ AE $ at 1 1 1 5 (Access (Fix $ AE $ at 1 1 1 2 (VarExpr (VarRef [] $ LowercaseIdentifier "x"))) (LowercaseIdentifier "f1"))) (LowercaseIdentifier "f2"))
        , testCase "does not allow symbolic field names" $
            assertParseFailure (expr Elm_0_19) "x.+"
        , testCase "does not allow symbolic field names" $
            assertParseFailure (expr Elm_0_19) "x.(+)"
        ]

    , testGroup "record access fuction"
        [ example "" ".f1" $ Fix $ AE $ at 1 1 1 4 $ AccessFunction (LowercaseIdentifier "f1")
        ]

    , testGroup "lambda"
        [ example "" "\\x y->9" $ Fix $ AE $ at 1 1 1 8 $ Lambda [([], at 1 2 1 3 $ P.VarPattern $ LowercaseIdentifier "x"), ([], at 1 4 1 5 $ P.VarPattern $ LowercaseIdentifier "y")] [] (intExpr (1,7,1,8) 9) False
        , example "single parameter" "\\x->9" $ Fix $ AE $ at 1 1 1 6 $ Lambda [([], at 1 2 1 3 $ P.VarPattern $ LowercaseIdentifier "x")] [] (intExpr (1,5,1,6) 9) False
        , example "whitespace" "\\ x y -> 9" $ Fix $ AE $ at 1 1 1 11 $ Lambda [([], at 1 3 1 4 $ P.VarPattern $ LowercaseIdentifier "x"), ([], at 1 5 1 6 $ P.VarPattern $ LowercaseIdentifier "y")] [] (intExpr (1,10,1,11) 9) False
        , example "comments" "\\{-A-}x{-B-}y{-C-}->{-D-}9" $ Fix $ AE $ at 1 1 1 27 $ Lambda [([BlockComment ["A"]], at 1 7 1 8 $ P.VarPattern $ LowercaseIdentifier "x"), ([BlockComment ["B"]], at 1 13 1 14 $ P.VarPattern $ LowercaseIdentifier "y")] [BlockComment ["C"], BlockComment ["D"]] (intExpr (1,26,1,27) 9) False
        , example "newlines" "\\\n x\n y\n ->\n 9" $ Fix $ AE $ at 1 1 5 3 $ Lambda [([], at 2 2 2 3 $ P.VarPattern $ LowercaseIdentifier "x"), ([], at 3 2 3 3 $ P.VarPattern $ LowercaseIdentifier "y")] [] (intExpr (5,2,5,3) 9) True
        , testCase "arrow must not contain whitespace" $
            assertParseFailure (expr Elm_0_19) "\\x y - > 9"
        ]

    , testGroup "if statement"
        [ example "" "if x then y else z" $ Fix $ AE $ at 1 1 1 19 (If (IfClause (Commented [] (Fix $ AE $ at 1 4 1 5 (VarExpr (VarRef [] $ LowercaseIdentifier "x"))) []) (Commented [] (Fix $ AE $ at 1 11 1 12 (VarExpr (VarRef [] $ LowercaseIdentifier "y"))) [])) [] ([],Fix $ AE $ at 1 18 1 19 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "comments" "if{-A-}x{-B-}then{-C-}y{-D-}else{-E-}if{-F-}x_{-G-}then{-H-}y_{-I-}else{-J-}z" $ Fix $ AE $ at 1 1 1 78 (If (IfClause (Commented [BlockComment ["A"]] (Fix $ AE $ at 1 8 1 9 (VarExpr (VarRef [] $ LowercaseIdentifier "x"))) [BlockComment ["B"]]) (Commented [BlockComment ["C"]] (Fix $ AE $ at 1 23 1 24 (VarExpr (VarRef [] $ LowercaseIdentifier "y"))) [BlockComment ["D"]])) [([BlockComment ["E"]],IfClause (Commented [BlockComment ["F"]] (Fix $ AE $ at 1 45 1 47 (VarExpr (VarRef [] $ LowercaseIdentifier "x_"))) [BlockComment ["G"]]) (Commented [BlockComment ["H"]] (Fix $ AE $ at 1 61 1 63 (VarExpr (VarRef [] $ LowercaseIdentifier "y_"))) [BlockComment ["I"]]))] ([BlockComment ["J"]],Fix $ AE $ at 1 77 1 78 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "else if" "if x1 then y1 else if x2 then y2 else if x3 then y3 else z" $ Fix $ AE $ at 1 1 1 59 (If (IfClause (Commented [] (Fix $ AE $ at 1 4 1 6 (VarExpr (VarRef [] $ LowercaseIdentifier "x1"))) []) (Commented [] (Fix $ AE $ at 1 12 1 14 (VarExpr (VarRef [] $ LowercaseIdentifier "y1"))) [])) [([],IfClause (Commented [] (Fix $ AE $ at 1 23 1 25 (VarExpr (VarRef [] $ LowercaseIdentifier "x2"))) []) (Commented [] (Fix $ AE $ at 1 31 1 33 (VarExpr (VarRef [] $ LowercaseIdentifier "y2"))) [])),([],IfClause (Commented [] (Fix $ AE $ at 1 42 1 44 (VarExpr (VarRef [] $ LowercaseIdentifier "x3"))) []) (Commented [] (Fix $ AE $ at 1 50 1 52 (VarExpr (VarRef [] $ LowercaseIdentifier "y3"))) []))] ([],Fix $ AE $ at 1 58 1 59 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "newlines" "if\n x\n then\n y\n else\n z" $ Fix $ AE $ at 1 1 6 3 (If (IfClause (Commented [] (Fix $ AE $ at 2 2 2 3 (VarExpr (VarRef [] $ LowercaseIdentifier "x"))) []) (Commented [] (Fix $ AE $ at 4 2 4 3 (VarExpr (VarRef [] $ LowercaseIdentifier "y"))) [])) [] ([],Fix $ AE $ at 6 2 6 3 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        ]

    , testGroup "let statement"
        [ example "" "let a=b in z" $ Fix $ AE $ at 1 1 1 13 (Let [LetDefinition (at 1 5 1 6 (P.VarPattern (LowercaseIdentifier "a"))) [] [] (Fix $ AE $ at 1 7 1 8 (VarExpr (VarRef [] $ LowercaseIdentifier "b")))] [] (Fix $ AE $ at 1 12 1 13 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "multiple declarations" "let a=b\n    c=d\nin z" $ Fix $ AE $ at 1 1 3 5 (Let [LetDefinition (at 1 5 1 6 (P.VarPattern (LowercaseIdentifier "a"))) [] [] (Fix $ AE $ at 1 7 1 8 (VarExpr (VarRef [] $ LowercaseIdentifier "b"))),LetDefinition (at 2 5 2 6 (P.VarPattern (LowercaseIdentifier "c"))) [] [] (Fix $ AE $ at 2 7 2 8 (VarExpr (VarRef [] $ LowercaseIdentifier "d")))] [] (Fix $ AE $ at 3 4 3 5 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "multiple declarations" "let\n a=b\n c=d\nin z" $ Fix $ AE $ at 1 1 4 5 (Let [LetDefinition (at 2 2 2 3 (P.VarPattern (LowercaseIdentifier "a"))) [] [] (Fix $ AE $ at 2 4 2 5 (VarExpr (VarRef [] $ LowercaseIdentifier "b"))),LetDefinition (at 3 2 3 3 (P.VarPattern (LowercaseIdentifier "c"))) [] [] (Fix $ AE $ at 3 4 3 5 (VarExpr (VarRef [] $ LowercaseIdentifier "d")))] [] (Fix $ AE $ at 4 4 4 5 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "whitespace" "let a = b in z" $ Fix $ AE $ at 1 1 1 15 (Let [LetDefinition (at 1 5 1 6 (P.VarPattern (LowercaseIdentifier "a"))) [] [] (Fix $ AE $ at 1 9 1 10 (VarExpr (VarRef [] $ LowercaseIdentifier "b")))] [] (Fix $ AE $ at 1 14 1 15 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "comments" "let{-A-}a{-B-}={-C-}b{-D-}in{-E-}z" $ Fix $ AE $ at 1 1 1 35 (Let [LetComment (BlockComment ["A"]),LetDefinition (at 1 9 1 10 (P.VarPattern (LowercaseIdentifier "a"))) [] [BlockComment ["B"],BlockComment ["C"]] (Fix $ AE $ at 1 21 1 22 (VarExpr (VarRef [] $ LowercaseIdentifier "b"))),LetComment (BlockComment ["D"])] [BlockComment ["E"]] (Fix $ AE $ at 1 34 1 35 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , example "newlines" "let\n a\n =\n b\nin\n z" $ Fix $ AE $ at 1 1 6 3 (Let [LetDefinition (at 2 2 2 3 (P.VarPattern (LowercaseIdentifier "a"))) [] [] (Fix $ AE $ at 4 2 4 3 (VarExpr (VarRef [] $ LowercaseIdentifier "b")))] [] (Fix $ AE $ at 6 2 6 3 (VarExpr (VarRef [] $ LowercaseIdentifier "z"))))
        , testCase "must have at least one definition" $
            assertParseFailure (expr Elm_0_19) "let in z"
        , testGroup "declarations must start at the same column" $
            [ testCase "(1)" $ assertParseFailure (expr Elm_0_19) "let a=b\n   c=d\nin z"
            , testCase "(2)" $ assertParseFailure (expr Elm_0_19) "let a=b\n     c=d\nin z"
            , testCase "(3)" $ assertParseFailure (expr Elm_0_19) "let  a=b\n   c=d\nin z"
            ]
        ]

    , testGroup "case statement"
        [ example "" "case 9 of\n 1->10\n _->20" $ Fix $ AE $ at 1 1 3 7 (Case (Commented [] (Fix $ AE $ at 1 6 1 7 (Literal (IntNum 9 DecimalInt))) [],False) [(Commented [] (at 2 2 2 3 (P.Literal (IntNum 1 DecimalInt))) [],([],Fix $ AE $ at 2 5 2 7 (Literal (IntNum 10 DecimalInt)))),(Commented [] (at 3 2 3 3 Anything) [],([],Fix $ AE $ at 3 5 3 7 (Literal (IntNum 20 DecimalInt))))])
        , example "no newline after 'of'" "case 9 of 1->10\n          _->20" $ Fix $ AE $ at 1 1 2 16 (Case (Commented [] (Fix $ AE $ at 1 6 1 7 (Literal (IntNum 9 DecimalInt))) [],False) [(Commented [] (at 1 11 1 12 (P.Literal (IntNum 1 DecimalInt))) [],([],Fix $ AE $ at 1 14 1 16 (Literal (IntNum 10 DecimalInt)))),(Commented [] (at 2 11 2 12 Anything) [],([],Fix $ AE $ at 2 14 2 16 (Literal (IntNum 20 DecimalInt))))])
        , example "whitespace" "case 9 of\n 1 -> 10\n _ -> 20" $ Fix $ AE $ at 1 1 3 9 (Case (Commented [] (Fix $ AE $ at 1 6 1 7 (Literal (IntNum 9 DecimalInt))) [],False) [(Commented [] (at 2 2 2 3 (P.Literal (IntNum 1 DecimalInt))) [],([],Fix $ AE $ at 2 7 2 9 (Literal (IntNum 10 DecimalInt)))),(Commented [] (at 3 2 3 3 Anything) [],([],Fix $ AE $ at 3 7 3 9 (Literal (IntNum 20 DecimalInt))))])
        , example "comments" "case{-A-}9{-B-}of{-C-}\n{-D-}1{-E-}->{-F-}10{-G-}\n{-H-}_{-I-}->{-J-}20" $ Fix $ AE $ at 1 1 3 21 (Case (Commented [BlockComment ["A"]] (Fix $ AE $ at 1 10 1 11 (Literal (IntNum 9 DecimalInt))) [BlockComment ["B"]],False) [(Commented [BlockComment ["C"],BlockComment ["D"]] (at 2 6 2 7 (P.Literal (IntNum 1 DecimalInt))) [BlockComment ["E"]],([BlockComment ["F"]],Fix $ AE $ at 2 19 2 21 (Literal (IntNum 10 DecimalInt)))),(Commented [BlockComment ["G"],BlockComment ["H"]] (at 3 6 3 7 Anything) [BlockComment ["I"]],([BlockComment ["J"]],Fix $ AE $ at 3 19 3 21 (Literal (IntNum 20 DecimalInt))))])
        , example "newlines" "case\n 9\n of\n 1\n ->\n 10\n _\n ->\n 20" $ Fix $ AE $ at 1 1 9 4 (Case (Commented [] (Fix $ AE $ at 2 2 2 3 (Literal (IntNum 9 DecimalInt))) [],True) [(Commented [] (at 4 2 4 3 (P.Literal (IntNum 1 DecimalInt))) [],([],Fix $ AE $ at 6 2 6 4 (Literal (IntNum 10 DecimalInt)))),(Commented [] (at 7 2 7 3 Anything) [],([],Fix $ AE $ at 9 2 9 4 (Literal (IntNum 20 DecimalInt))))])
        , testCase "should not consume trailing whitespace" $
            assertParse (expr Elm_0_19>> string "\nX") "case 9 of\n 1->10\n _->20\nX" $ "\nX"
        , testGroup "clauses must start at the same column"
            [ testCase "(1)" $ assertParseFailure (expr Elm_0_19) "case 9 of\n 1->10\n_->20"
            , testCase "(2)" $ assertParseFailure (expr Elm_0_19) "case 9 of\n 1->10\n  _->20"
            , testCase "(3)" $ assertParseFailure (expr Elm_0_19) "case 9 of\n  1->10\n _->20"
            ]
        ]
    ]
