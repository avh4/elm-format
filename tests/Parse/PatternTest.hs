module Parse.PatternTest where

import Test.Tasty
import Test.Tasty.HUnit

import Parse.Pattern
import AST.V0_16
import AST.Pattern
import ElmVersion

import Parse.TestHelpers


example :: String -> String -> Pattern -> TestTree
example name input expected =
    testCase name $
        assertParse (expr Elm_0_19) input expected


tests :: TestTree
tests =
    testGroup "Parse.Pattern"
    [ example "wildcard" "_" $ at 1 1 1 2 Anything

    , example "literal" "1" $ at 1 1 1 2 (Literal (IntNum 1 DecimalInt))

    , example "variable" "a" $ at 1 1 1 2 (VarPattern (LowercaseIdentifier "a"))

    , testGroup "data"
        [ example "" "Just x y" $ at 1 1 1 9 (Data [UppercaseIdentifier "Just"] [([],at 1 6 1 7 (VarPattern (LowercaseIdentifier "x"))),([],at 1 8 1 9 (VarPattern (LowercaseIdentifier "y")))])
        , example "single parameter" "Just x" $ at 1 1 1 7 (Data [UppercaseIdentifier "Just"] [([],at 1 6 1 7 (VarPattern (LowercaseIdentifier "x")))])
        , example "comments" "Just{-A-}x{-B-}y" $ at 1 1 1 17 (Data [UppercaseIdentifier "Just"] [([BlockComment ["A"]],at 1 10 1 11 (VarPattern (LowercaseIdentifier "x"))),([BlockComment ["B"]],at 1 16 1 17 (VarPattern (LowercaseIdentifier "y")))])
        , example "newlines" "Just\n x\n y" $ at 1 1 3 3 (Data [UppercaseIdentifier "Just"] [([],at 2 2 2 3 (VarPattern (LowercaseIdentifier "x"))),([],at 3 2 3 3 (VarPattern (LowercaseIdentifier "y")))])
        ]

    , testGroup "unit"
        [ example "" "()" $ at 1 1 1 3 (UnitPattern [])
        , example "whitespace" "( )" $ at 1 1 1 4 (UnitPattern [])
        , example "comments" "({-A-})" $ at 1 1 1 8 (UnitPattern [BlockComment ["A"]])
        , example "newlines" "(\n )" $ at 1 1 2 3 (UnitPattern [])
        ]

    , testGroup "parentheses"
        [ example "" "(_)" $ at 1 2 1 3 Anything
        , example "whitespace" "( _ )" $ at 1 3 1 4 Anything
        , example "comments" "({-A-}_{-B-})" $ at 1 1 1 14 (PatternParens (Commented [BlockComment ["A"]] (at 1 7 1 8 Anything) [BlockComment ["B"]]))
        , example "newlines" "(\n _\n )" $ at 2 2 2 3 Anything
        ]

    , testGroup "tuple"
        [ example "" "(x,y)" $ at 1 1 1 6 (Tuple [Commented [] (at 1 2 1 3 (VarPattern (LowercaseIdentifier "x"))) [],Commented [] (at 1 4 1 5 (VarPattern (LowercaseIdentifier "y"))) []])
        , example "whitespace" "( x , y )" $ at 1 1 1 10 (Tuple [Commented [] (at 1 3 1 4 (VarPattern (LowercaseIdentifier "x"))) [],Commented [] (at 1 7 1 8 (VarPattern (LowercaseIdentifier "y"))) []])
        , example "comments" "({-A-}x{-B-},{-C-}y{-D-})" $ at 1 1 1 26 (Tuple [Commented [BlockComment ["A"]] (at 1 7 1 8 (VarPattern (LowercaseIdentifier "x"))) [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (VarPattern (LowercaseIdentifier "y"))) [BlockComment ["D"]]])
        , example "newlines" "(\n x\n ,\n y\n )" $ at 1 1 5 3 (Tuple [Commented [] (at 2 2 2 3 (VarPattern (LowercaseIdentifier "x"))) [],Commented [] (at 4 2 4 3 (VarPattern (LowercaseIdentifier "y"))) []])
        ]

    , testGroup "empty list pattern"
        [ example "" "[]" $ at 1 1 1 3 (EmptyListPattern [])
        , example "whitespace" "[ ]" $ at 1 1 1 4 (EmptyListPattern [])
        , example "comments" "[{-A-}]" $ at 1 1 1 8 (EmptyListPattern [BlockComment ["A"]])
        , example "newlines" "[\n ]" $ at 1 1 2 3 (EmptyListPattern [])
        ]

    , testGroup "list"
        [ example "" "[x,y]" $ at 1 1 1 6 (List [Commented [] (at 1 2 1 3 (VarPattern (LowercaseIdentifier "x"))) [],Commented [] (at 1 4 1 5 (VarPattern (LowercaseIdentifier "y"))) []])
        , example "single element" "[x]" $ at 1 1 1 4 (List [Commented [] (at 1 2 1 3 (VarPattern (LowercaseIdentifier "x"))) []])
        , example "whitespace" "[ x , y ]" $ at 1 1 1 10 (List [Commented [] (at 1 3 1 4 (VarPattern (LowercaseIdentifier "x"))) [],Commented [] (at 1 7 1 8 (VarPattern (LowercaseIdentifier "y"))) []])
        , example "comments" "[{-A-}x{-B-},{-C-}y{-D-}]" $ at 1 1 1 26 (List [Commented [BlockComment ["A"]] (at 1 7 1 8 (VarPattern (LowercaseIdentifier "x"))) [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (VarPattern (LowercaseIdentifier "y"))) [BlockComment ["D"]]])
        , example "newlines" "[\n x\n ,\n y\n ]" $ at 1 1 5 3 (List [Commented [] (at 2 2 2 3 (VarPattern (LowercaseIdentifier "x"))) [],Commented [] (at 4 2 4 3 (VarPattern (LowercaseIdentifier "y"))) []])
        ]

    , testGroup "record"
        [ example "" "{a,b}" $ at 1 1 1 6 (Record [Commented [] (LowercaseIdentifier "a") [],Commented [] (LowercaseIdentifier "b") []])
        , example "single element" "{a}" $ at 1 1 1 4 (Record [Commented [] (LowercaseIdentifier "a") []])
        , example "whitespace" "{ a , b }" $ at 1 1 1 10 (Record [Commented [] (LowercaseIdentifier "a") [],Commented [] (LowercaseIdentifier "b") []])
        , example "comments" "{{-A-}a{-B-},{-C-}b{-D-}}" $ at 1 1 1 26 (Record [Commented [BlockComment ["A"]] (LowercaseIdentifier "a") [BlockComment ["B"]],Commented [BlockComment ["C"]] (LowercaseIdentifier "b") [BlockComment ["D"]]])
        , example "newlines" "{\n a\n ,\n b\n }" $ at 1 1 5 3 (Record [Commented [] (LowercaseIdentifier "a") [],Commented [] (LowercaseIdentifier "b") []])
        , example "empty" "{}" $ at 1 1 1 3 (EmptyRecordPattern [])
        ]

    , testGroup "alias"
        [ example "" "_ as x" $ at 1 1 1 7 (Alias (at 1 1 1 2 Anything,[]) ([],LowercaseIdentifier "x"))
        , example "left side has whitespace" "A b as x" $ at 1 1 1 9 (Alias (at 1 1 1 4 (Data [UppercaseIdentifier "A"] [([], at 1 3 1 4 (VarPattern (LowercaseIdentifier "b")))]),[]) ([],LowercaseIdentifier "x"))
        , example "left side ctor without whitespace" "A as x" $ at 1 1 1 7 (Alias (at 1 1 1 2 (Data [UppercaseIdentifier "A"] []),[]) ([],LowercaseIdentifier "x"))
        , example "comments" "_{-A-}as{-B-}x" $ at 1 1 1 15 (Alias (at 1 1 1 2 Anything,[BlockComment ["A"]]) ([BlockComment ["B"]],LowercaseIdentifier "x"))
        , example "newlines" "_\n as\n x" $ at 1 1 3 3 (Alias (at 1 1 1 2 Anything,[]) ([],LowercaseIdentifier "x"))
        , example "nested" "(_ as x)as y" $ at 1 1 1 13 (Alias (at 1 2 1 8 (Alias (at 1 2 1 3 Anything,[]) ([],LowercaseIdentifier "x")),[]) ([],LowercaseIdentifier "y"))
        , example "nested (whitespace)" "(_ as x) as y" $ at 1 1 1 14 (Alias (at 1 2 1 8 (Alias (at 1 2 1 3 Anything,[]) ([],LowercaseIdentifier "x")),[]) ([],LowercaseIdentifier "y"))
        , testCase "nesting required parentheses" $
            assertParseFailure (expr Elm_0_19) "_ as x as y"
        ]
    ]
