{-# LANGUAGE DataKinds #-}
module Parse.ExpressionTest where

import Test.Tasty
import Test.Tasty.HUnit

import Parse.Expression
import AST.V0_16
import AST.Structure
import qualified Box
import qualified Data.Bimap as Bimap
import Data.Coapplicative
import qualified Data.Indexed as I
import qualified Data.Text as Text
import Parse.ParsecAdapter (string)
import ElmFormat.ImportInfo (ImportInfo(..))
import ElmFormat.Render.Box (formatExpression, syntaxParens, SyntaxContext (SyntaxSeparated))
import ElmVersion
import Parse.TestHelpers
import Reporting.Annotation (Located)
import Data.Word (Word16)
import GHC.Int (Int64)
import qualified ElmFormat.Render.ElmStructure as ElmStructure
import qualified Data.Fix as Fix


pending :: I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'ExpressionNK
pending = at 0 0 0 0 $ Unit []


example :: String -> String -> I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'ExpressionNK -> TestTree
example name input expected =
    testCase name $
        assertParse (expr Elm_0_19) input expected


importInfo :: Ord ns => ImportInfo ns
importInfo =
    ImportInfo mempty Bimap.empty mempty mempty mempty


example' :: String -> String -> String -> TestTree
example' name input expected =
    testCase name $
        assertParse (fmap (Text.unpack . Box.render . Fix.cata ElmStructure.render . syntaxParens SyntaxSeparated . formatExpression Elm_0_19 importInfo . I.fold2 (I.Fix . extract)) (expr Elm_0_19)) input expected


commentedIntExpr :: (Word16, Word16, Word16, Word16) -> String -> String -> Int64 -> Commented ([Comment], [Comment]) (I.Fix2 Located (ASTNS ns) 'ExpressionNK)
commentedIntExpr (a,b,c,d) preComment postComment i =
    C ([BlockComment [preComment]], [BlockComment [postComment]]) (at a b c d  $ Literal $ IntNum i DecimalInt)

commentedIntExpr' :: (Word16, Word16, Word16, Word16) -> String -> Int64 -> Commented ([Comment], [a]) (I.Fix2 Located (ASTNS ns) 'ExpressionNK)
commentedIntExpr' (a,b,c,d) preComment i =
    C ([BlockComment [preComment]], []) (at a b c d  $ Literal $ IntNum i DecimalInt)


commentedIntExpr'' :: (Word16, Word16, Word16, Word16) -> String -> Int64 -> Commented [Comment] (I.Fix2 Located (ASTNS ns) 'ExpressionNK)
commentedIntExpr'' (a,b,c,d) preComment i =
    C [BlockComment [preComment]] $ at a b c d  $ Literal $ IntNum i DecimalInt


intExpr :: (Word16, Word16, Word16, Word16) -> Int64 -> I.Fix2 Located (ASTNS ns) 'ExpressionNK
intExpr (a,b,c,d) i = at a b c d $ Literal $ IntNum i DecimalInt

intExpr' :: (Word16, Word16, Word16, Word16) -> Int64 -> Commented ([a1], [a2]) (I.Fix2 Located (ASTNS ns) 'ExpressionNK)
intExpr' (a,b,c,d) i =
    C ([], []) (at a b c d  $ Literal $ IntNum i DecimalInt)

intExpr'' :: (Word16, Word16, Word16, Word16) -> Int64 -> Commented [a] (I.Fix2 Located (ASTNS ns) 'ExpressionNK)
intExpr'' (a,b,c,d) i =
    C [] $ at a b c d  $ Literal $ IntNum i DecimalInt


test_tests :: TestTree
test_tests =
    testGroup "Parse.Expression"
    [ testGroup "Unit"
        [ example "" "()" $ at 1 1 1 3 $ Unit []
        , example "whitespace" "( )" $ at 1 1 1 4 $ Unit []
        , example "comments" "({-A-})" $ at 1 1 1 8 $ Unit [BlockComment ["A"]]
        , example "newlines" "(\n )" $ at 1 1 2 3 $ Unit []
        ]

    , testGroup "Literal"
        [ example "" "1" $ at 1 1 1 2 (Literal (IntNum 1 DecimalInt))

        , testGroup "Boolean"
            [ example "True" "True" $ at 1 1 1 5 $ Literal $ Boolean True
            , example "False" "False" $ at 1 1 1 6 $ Literal $ Boolean False
            ]
        ]

    , testGroup "variable"
        [ example "lowercase" "foo" $ at 1 1 1 4 $ VarExpr $ at 1 1 1 4 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "foo"
        , example "uppercase" "Bar" $ at 1 1 1 4 $ VarExpr $ at 1 1 1 4 $ VarRef_ $ TagRef [] $ UppercaseIdentifier "Bar"
        , example "qualified" "Bar.Baz.foo" $ at 1 1 1 12 $ VarExpr $ at 1 1 1 12 $ VarRef_ $ VarRef [UppercaseIdentifier "Bar", UppercaseIdentifier "Baz"] $ LowercaseIdentifier "foo"

        , testGroup "symbolic operator"
            [ example "" "(+)" $ at 1 1 1 4 $ VarExpr $ at 1 2 1 3 $ VarRef_ $ OpRef (SymbolIdentifier "+")
            , testCase "does not allow whitespace" $
                assertParseFailure (expr Elm_0_19) "( + )"
            , testCase "doew not allow comments" $
                assertParseFailure (expr Elm_0_19) "({-A-}+{-B-})"
            ]
        ]

    , testGroup "function application"
        [ example "" "f 7 8" $ at 1 1 1 6 $ App (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (1,3,1,4) 7, intExpr'' (1,5,1,6) 8] (FAJoinFirst JoinAll)
        , example "argument starts with minus" "f -9 -x" $ at 1 1 1 8 $ App (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (1,3,1,5) (-9), C [] $ at 1 6 1 8 $ Unary Negative $ at 1 7 1 8 $ VarExpr $ at 1 7 1 8 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x"] (FAJoinFirst JoinAll)
        , example "comments" "f{-A-}7{-B-}8" $ at 1 1 1 14 $ App (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "f") [commentedIntExpr'' (1,7,1,8) "A" 7, commentedIntExpr'' (1,13,1,14) "B" 8] (FAJoinFirst JoinAll)
        , example "newlines (1)" "f 7\n 8" $ at 1 1 2 3 $ App (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (1,3,1,4) 7, intExpr'' (2,2,2,3) 8] (FAJoinFirst SplitAll)
        , example "newlines (2)" "f\n 7\n 8" $ at 1 1 3 3 $ App (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "f") [intExpr'' (2,2,2,3) 7, intExpr'' (3,2,3,3) 8] FASplitFirst
        , example "newlines and comments" "f\n {-A-}7\n {-B-}8" $ at 1 1 3 8 $ App (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "f") [commentedIntExpr'' (2,7,2,8) "A" 7, commentedIntExpr'' (3,7,3,8) "B" 8] FASplitFirst
        ]

    , testGroup "unary operators"
        [ testGroup "negative"
            [ example "" "-True" $ at 1 1 1 6 $ Unary Negative $ at 1 2 1 6 $ Literal $ Boolean True
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
        [ example "" "7+8<<>>9" $ at 1 1 1 9 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (at 1 2 1 3 $ VarRef_ $ OpRef $ SymbolIdentifier "+") [] (intExpr (1,3,1,4) 8), BinopsClause [] (at 1 4 1 8 $ VarRef_ $ OpRef $ SymbolIdentifier "<<>>") [] (intExpr (1,8,1,9) 9)] False
        , example "minus with no whitespace" "9-1" $ at 1 1 1 4 $ Binops (intExpr (1,1,1,2) 9) [BinopsClause [] (at 1 2 1 3 $ VarRef_ $ OpRef $ SymbolIdentifier "-") [] (intExpr (1,3,1,4) 1)] False
        , example "backticks" "7`plus`8`shift`9" $ at 1 1 1 17 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (at 1 2 1 8 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "plus") [] (intExpr (1,8,1,9) 8), BinopsClause [] (at 1 9 1 16 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "shift") [] (intExpr (1,16,1,17) 9)] False
        , example "whitespace" "7 + 8 <<>> 9" $ at 1 1 1 13 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (at 1 3 1 4 $ VarRef_ $ OpRef $ SymbolIdentifier "+") [] (intExpr (1,5,1,6) 8), BinopsClause [] (at 1 7 1 11 $ VarRef_ $ OpRef $ SymbolIdentifier "<<>>") [] (intExpr (1,12,1,13) 9)] False
        , example "comments" "7{-A-}+{-B-}8{-C-}<<>>{-D-}9" $ at 1 1 1 29 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [BlockComment ["A"]] (at 1 7 1 8 $ VarRef_ $ OpRef $ SymbolIdentifier "+") [BlockComment ["B"]] (intExpr (1,13,1,14) 8), BinopsClause [BlockComment ["C"]] (at 1 19 1 23 $ VarRef_ $ OpRef $ SymbolIdentifier "<<>>") [BlockComment ["D"]] (intExpr (1,28,1,29) 9)] False
        , example "newlines" "7\n +\n 8\n <<>>\n 9" $ at 1 1 5 3 $ Binops (intExpr (1,1,1,2) 7) [BinopsClause [] (at 2 2 2 3 $ VarRef_ $ OpRef $ SymbolIdentifier "+") [] (intExpr (3,2,3,3) 8), BinopsClause [] (at 4 2 4 6 $ VarRef_ $ OpRef $ SymbolIdentifier "<<>>") [] (intExpr (5,2,5,3) 9)] True
        ]

    , testGroup "parentheses"
        [ example "" "(1)" $ at 1 1 1 4 $ Parens $ intExpr' (1,2,1,3) 1
        , example "whitespace" "( 1 )" $ at 1 1 1 6 $ Parens $ intExpr' (1,3,1,4) 1
        , example "comments" "({-A-}1{-B-})" $ at 1 1 1 14 $ Parens $ commentedIntExpr (1,7,1,8) "A" "B" 1
        , example "newlines" "(\n 1\n )" $ at 1 1 3 3 $ Parens $ intExpr' (2,2,2,3) 1
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
        , example' "sections with multiple items"
            "[{-A-}1{-B-},{-C-}2,{-D-}3]"
            "[ {- A -} 1\n\
            \\n\
            \{- B -}\n\
            \, {- C -} 2\n\
            \, {- D -} 3\n\
            \]\n"
        , example' "newlines"
            "[\n 1\n ,\n 2\n ,\n 3\n ]"
            "[ 1\n, 2\n, 3\n]\n"
        ]

    , testGroup "Range"
        [ example "" "[7..9]" $ at 1 1 1 7 $ Range (intExpr' (1,2,1,3) 7) (intExpr' (1,5,1,6) 9)
        , example "whitespace" "[ 7 .. 9 ]" $ at 1 1 1 11 $ Range (intExpr' (1,3,1,4) 7) (intExpr' (1,8,1,9) 9)
        , example "comments" "[{-A-}7{-B-}..{-C-}9{-D-}]" $ at 1 1 1 27 $ Range (commentedIntExpr (1,7,1,8) "A" "B" 7) (commentedIntExpr (1,20,1,21) "C" "D" 9)
        , example "newlines" "[\n 7\n ..\n 9\n ]" $ at 1 1 5 3 $ Range (intExpr' (2,2,2,3) 7) (intExpr' (4,2,4,3) 9)
        ]

    , testGroup "Tuple"
        [ example "" "(1,2)" $ at 1 1 1 6 $ Tuple [intExpr' (1,2,1,3) 1, intExpr' (1,4,1,5) 2] False
        , example "whitespace" "( 1 , 2 )" $ at 1 1 1 10 $ Tuple [intExpr' (1,3,1,4) 1, intExpr' (1,7,1,8) 2] False
        , example "comments" "({-A-}1{-B-},{-C-}2{-D-})" $ at 1 1 1 26 $ Tuple [commentedIntExpr (1,7,1,8) "A" "B" 1, commentedIntExpr (1,19,1,20) "C" "D" 2] False
        , example "newlines" "(\n 1\n ,\n 2\n )" $ at 1 1 5 3 $ Tuple [intExpr' (2,2,2,3) 1, intExpr' (4,2,4,3) 2] True
        ]

    , testGroup "tuple constructor"
        [ example "" "(,,)" $ at 1 1 1 5 $ TupleFunction 3
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
        , example' "comments + multiline"
            "{{-A-}a{-B-}|\n{-C-}x{-D-}={-E-}7{-F-},{-G-}y{-H-}={-I-}8{-J-}}"
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
            "{ a | }\n"
        ]

    , testGroup "record access"
        [ example "" "x.f1" $ at 1 1 1 5 (Access (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x") (LowercaseIdentifier "f1"))
        , example "nested" "x.f1.f2" $ at 1 1 1 8 (Access (at 1 1 1 5 (Access (at 1 1 1 2 $ VarExpr $ at 1 1 1 2 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x") (LowercaseIdentifier "f1"))) (LowercaseIdentifier "f2"))
        , testCase "does not allow symbolic field names" $
            assertParseFailure (expr Elm_0_19) "x.+"
        , testCase "does not allow symbolic field names" $
            assertParseFailure (expr Elm_0_19) "x.(+)"
        ]

    , testGroup "record access fuction"
        [ example "" ".f1" $ at 1 1 1 4 $ AccessFunction (LowercaseIdentifier "f1")
        ]

    , testGroup "lambda"
        [ example "" "\\x y->9" $ at 1 1 1 8 $ Lambda [C [] ( at 1 2 1 3 $ VarPattern $ LowercaseIdentifier "x"), C [] ( at 1 4 1 5 $ VarPattern $ LowercaseIdentifier "y")] [] (intExpr (1,7,1,8) 9) False
        , example "single parameter" "\\x->9" $ at 1 1 1 6 $ Lambda [C [] ( at 1 2 1 3 $ VarPattern $ LowercaseIdentifier "x")] [] (intExpr (1,5,1,6) 9) False
        , example "whitespace" "\\ x y -> 9" $ at 1 1 1 11 $ Lambda [C [] ( at 1 3 1 4 $ VarPattern $ LowercaseIdentifier "x"), C [] ( at 1 5 1 6 $ VarPattern $ LowercaseIdentifier "y")] [] (intExpr (1,10,1,11) 9) False
        , example "comments" "\\{-A-}x{-B-}y{-C-}->{-D-}9" $ at 1 1 1 27 $ Lambda [C [BlockComment ["A"]] ( at 1 7 1 8 $ VarPattern $ LowercaseIdentifier "x"), C [BlockComment ["B"]] ( at 1 13 1 14 $ VarPattern $ LowercaseIdentifier "y")] [BlockComment ["C"], BlockComment ["D"]] (intExpr (1,26,1,27) 9) False
        , example "newlines" "\\\n x\n y\n ->\n 9" $ at 1 1 5 3 $ Lambda [C [] ( at 2 2 2 3 $ VarPattern $ LowercaseIdentifier "x"), C [] ( at 3 2 3 3 $ VarPattern $ LowercaseIdentifier "y")] [] (intExpr (5,2,5,3) 9) True
        , testCase "arrow must not contain whitespace" $
            assertParseFailure (expr Elm_0_19) "\\x y - > 9"
        ]

    , testGroup "if statement"
        [ example "" "if x then y else z" $ at 1 1 1 19 (If (IfClause (C ([], []) (at 1 4 1 5 $ VarExpr $ at 1 4 1 5 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x")) (C ([], []) (at 1 11 1 12 $ VarExpr $ at 1 11 1 12 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "y"))) [] (C [] (at 1 18 1 19 $ VarExpr $ at 1 18 1 19 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z")))
        , example "comments" "if{-A-}x{-B-}then{-C-}y{-D-}else{-E-}if{-F-}x_{-G-}then{-H-}y_{-I-}else{-J-}z" $ at 1 1 1 78 (If (IfClause (C ([BlockComment ["A"]], [BlockComment ["B"]]) (at 1 8 1 9 $ VarExpr $ at 1 8 1 9 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x")) (C ([BlockComment ["C"]], [BlockComment ["D"]]) (at 1 23 1 24 $ VarExpr $ at 1 23 1 24 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "y"))) [C [BlockComment ["E"]] (IfClause (C ([BlockComment ["F"]], [BlockComment ["G"]]) (at 1 45 1 47 $ VarExpr $ at 1 45 1 47 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x_")) (C ([BlockComment ["H"]], [BlockComment ["I"]]) (at 1 61 1 63 $ VarExpr $ at 1 61 1 63 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "y_")))] (C [BlockComment ["J"]] (at 1 77 1 78 $ VarExpr $ at 1 77 1 78 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z")))
        , example "else if" "if x1 then y1 else if x2 then y2 else if x3 then y3 else z" $ at 1 1 1 59 (If (IfClause (C ([], []) (at 1 4 1 6 $ VarExpr $ at 1 4 1 6 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x1")) (C ([], []) (at 1 12 1 14 $ VarExpr $ at 1 12 1 14 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "y1"))) [C [] (IfClause (C ([], []) (at 1 23 1 25 $ VarExpr $ at 1 23 1 25 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x2")) (C ([], []) (at 1 31 1 33 $ VarExpr $ at 1 31 1 33 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "y2"))),C [] (IfClause (C ([], []) (at 1 42 1 44 $ VarExpr $ at 1 42 1 44 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x3")) (C ([], []) (at 1 50 1 52 $ VarExpr $ at 1 50 1 52 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "y3")))] (C [] (at 1 58 1 59 $ VarExpr $ at 1 58 1 59 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z")))
        , example "newlines" "if\n x\n then\n y\n else\n z" $ at 1 1 6 3 (If (IfClause (C ([], []) (at 2 2 2 3 $ VarExpr $ at 2 2 2 3 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "x")) (C ([], []) (at 4 2 4 3 $ VarExpr $ at 4 2 4 3 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "y"))) [] (C [] $ at 6 2 6 3 $ VarExpr $ at 6 2 6 3 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z"))
        ]

    , testGroup "let statement"
        [ example "" "let a=b in z" $ at 1 1 1 13 (Let [at 1 5 1 8 $ LetCommonDeclaration $ at 1 5 1 8 $ Definition (at 1 5 1 6 (VarPattern (LowercaseIdentifier "a"))) [] [] (at 1 7 1 8 $ VarExpr $ at 1 7 1 8 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "b")] [] (at 1 12 1 13 $ VarExpr $ at 1 12 1 13 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z"))
        , example "multiple declarations" "let a=b\n    c=d\nin z" $ at 1 1 3 5 (Let [at 1 5 1 8 $ LetCommonDeclaration $ at 1 5 1 8 $ Definition (at 1 5 1 6 (VarPattern (LowercaseIdentifier "a"))) [] [] (at 1 7 1 8 $ VarExpr $ at 1 7 1 8 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "b"),at 2 5 2 8 $ LetCommonDeclaration $ at 2 5 2 8 $ Definition (at 2 5 2 6 (VarPattern (LowercaseIdentifier "c"))) [] [] (at 2 7 2 8 $ VarExpr $ at 2 7 2 8 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "d")] [] (at 3 4 3 5 $ VarExpr $ at 3 4 3 5 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z"))
        , example "multiple declarations" "let\n a=b\n c=d\nin z" $ at 1 1 4 5 (Let [at 2 2 2 5 $ LetCommonDeclaration $ at 2 2 2 5 $ Definition (at 2 2 2 3 (VarPattern (LowercaseIdentifier "a"))) [] [] (at 2 4 2 5 $ VarExpr $ at 2 4 2 5 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "b"),at 3 2 3 5 $ LetCommonDeclaration $ at 3 2 3 5 $ Definition (at 3 2 3 3 (VarPattern (LowercaseIdentifier "c"))) [] [] (at 3 4 3 5 $ VarExpr $ at 3 4 3 5 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "d")] [] (at 4 4 4 5 $ VarExpr $ at 4 4 4 5 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z"))
        , example "whitespace" "let a = b in z" $ at 1 1 1 15 (Let [at 1 5 1 10 $ LetCommonDeclaration $ at 1 5 1 10 $ Definition (at 1 5 1 6 (VarPattern (LowercaseIdentifier "a"))) [] [] (at 1 9 1 10 $ VarExpr $ at 1 9 1 10 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "b")] [] (at 1 14 1 15 $ VarExpr $ at 1 14 1 15 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z"))
        , example "comments" "let{-A-}a{-B-}={-C-}b{-D-}in{-E-}z" $ at 1 1 1 35 (Let [at 1 4 1 9 $ LetComment (BlockComment ["A"]),at 1 9 1 22 $ LetCommonDeclaration $ at 1 9 1 22 $ Definition (at 1 9 1 10 (VarPattern (LowercaseIdentifier "a"))) [] [BlockComment ["B"],BlockComment ["C"]] (at 1 21 1 22 $ VarExpr $ at 1 21 1 22 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "b"),at 1 22 1 27 $ LetComment (BlockComment ["D"])] [BlockComment ["E"]] (at 1 34 1 35 $ VarExpr $ at 1 34 1 35 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z"))
        , example "newlines" "let\n a\n =\n b\nin\n z" $ at 1 1 6 3 (Let [at 2 2 4 3 $ LetCommonDeclaration $ at 2 2 4 3 $ Definition (at 2 2 2 3 (VarPattern (LowercaseIdentifier "a"))) [] [] (at 4 2 4 3 $ VarExpr $ at 4 2 4 3 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "b")] [] (at 6 2 6 3 $ VarExpr $ at 6 2 6 3 $ VarRef_ $ VarRef [] $ LowercaseIdentifier "z"))
        , testCase "must have at least one definition" $
            assertParseFailure (expr Elm_0_19) "let in z"
        , testGroup "declarations must start at the same column" $
            [ testCase "(1)" $ assertParseFailure (expr Elm_0_19) "let a=b\n   c=d\nin z"
            , testCase "(2)" $ assertParseFailure (expr Elm_0_19) "let a=b\n     c=d\nin z"
            , testCase "(3)" $ assertParseFailure (expr Elm_0_19) "let  a=b\n   c=d\nin z"
            ]
        ]

    , testGroup "case statement"
        [ example "" "case 9 of\n 1->10\n _->20" $ at 1 1 3 7 (Case (C ([], []) (at 1 6 1 7 (Literal (IntNum 9 DecimalInt))),False) [at 2 2 2 7 $ CaseBranch [] [] [] (at 2 2 2 3 $ LiteralPattern $ IntNum 1 DecimalInt) (at 2 5 2 7 $ Literal $ IntNum 10 DecimalInt), at 2 7 3 7 $ CaseBranch [] [] [] (at 3 2 3 3 Anything) (at 3 5 3 7 $ Literal $ IntNum 20 DecimalInt)])
        , example "no newline after 'of'" "case 9 of 1->10\n          _->20" $ at 1 1 2 16 (Case (C ([], []) (at 1 6 1 7 (Literal (IntNum 9 DecimalInt))),False) [at 1 11 1 16 $ CaseBranch []  [] [] (at 1 11 1 12 $ LiteralPattern $ IntNum 1 DecimalInt) (at 1 14 1 16 $ Literal $ IntNum 10 DecimalInt), at 1 16 2 16 $ CaseBranch [] [] [] (at 2 11 2 12 Anything) (at 2 14 2 16 $ Literal $ IntNum 20 DecimalInt)])
        , example "whitespace" "case 9 of\n 1 -> 10\n _ -> 20" $ at 1 1 3 9 (Case (C ([], []) (at 1 6 1 7 (Literal (IntNum 9 DecimalInt))),False) [at 2 2 2 9 $ CaseBranch [] [] [] (at 2 2 2 3 $ LiteralPattern $ IntNum 1 DecimalInt) (at 2 7 2 9 $ Literal $ IntNum 10 DecimalInt), at 2 9 3 9 $ CaseBranch [] [] [] (at 3 2 3 3 Anything) (at 3 7 3 9 $ Literal $ IntNum 20 DecimalInt)])
        , example "comments" "case{-A-}9{-B-}of{-C-}\n{-D-}1{-E-}->{-F-}10{-G-}\n{-H-}_{-I-}->{-J-}20" $ at 1 1 3 21 (Case (C ([BlockComment ["A"]], [BlockComment ["B"]]) (at 1 10 1 11 (Literal (IntNum 9 DecimalInt))),False) [at 2 6 2 21 $ CaseBranch [BlockComment ["C"],BlockComment ["D"]] [BlockComment ["E"]] [BlockComment ["F"]] (at 2 6 2 7 $ LiteralPattern $ IntNum 1 DecimalInt) (at 2 19 2 21 $ Literal $ IntNum 10 DecimalInt), at 2 21 3 21 $ CaseBranch [BlockComment ["G"],BlockComment ["H"]] [BlockComment ["I"]] [BlockComment ["J"]] (at 3 6 3 7 Anything) (at 3 19 3 21 $ Literal $ IntNum 20 DecimalInt)])
        , example "newlines" "case\n 9\n of\n 1\n ->\n 10\n _\n ->\n 20" $ at 1 1 9 4 (Case (C ([], []) (at 2 2 2 3 (Literal (IntNum 9 DecimalInt))),True) [at 4 2 6 4 $ CaseBranch [] [] [] (at 4 2 4 3 $ LiteralPattern $ IntNum 1 DecimalInt) (at 6 2 6 4 $ Literal $ IntNum 10 DecimalInt), at 6 4 9 4 $ CaseBranch [] [] [] (at 7 2 7 3 Anything) (at 9 2 9 4 $ Literal $ IntNum 20 DecimalInt)])
        , testCase "should not consume trailing whitespace" $
            assertParse (expr Elm_0_19>> string "\nX") "case 9 of\n 1->10\n _->20\nX" $ "\nX"
        , testGroup "clauses must start at the same column"
            [ testCase "(1)" $ assertParseFailure (expr Elm_0_19) "case 9 of\n 1->10\n_->20"
            , testCase "(2)" $ assertParseFailure (expr Elm_0_19) "case 9 of\n 1->10\n  _->20"
            , testCase "(3)" $ assertParseFailure (expr Elm_0_19) "case 9 of\n  1->10\n _->20"
            ]
        ]
    ]
