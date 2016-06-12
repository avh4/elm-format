module Parse.PatternTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as LazyText

import Parse.Pattern
import Parse.Helpers (IParser, iParse)
import AST.V0_16
import AST.Pattern
import AST.Variable hiding (Alias)
import Reporting.Annotation hiding (map, at)
import Reporting.Region
import Text.Parsec.Char (string)
import Debug.Trace

import Parse.TestHelpers


pending = at 0 0 0 0 Anything

example name input expected =
    testCase name $
        assertParse expr input expected


tests :: TestTree
tests =
    testGroup "Parse.Pattern"
    [ example "wildcard" "_" $ at 1 1 1 2 Anything

    , example "literal" "1" $ at 1 1 1 2 (Literal (IntNum 1 DecimalInt))

    , example "variable" "a" $ at 1 1 1 2 (Var (VarRef "a"))

    , testGroup "data"
        [ example "" "Just x y" $ at 1 1 1 9 (Data "Just" [([],at 1 6 1 7 (Var (VarRef "x"))),([],at 1 8 1 9 (Var (VarRef "y")))])
        , example "single parameter" "Just x" $ at 1 1 1 7 (Data "Just" [([],at 1 6 1 7 (Var (VarRef "x")))])
        , example "comments" "Just{-A-}x{-B-}y" $ at 1 1 1 17 (Data "Just" [([BlockComment ["A"]],at 1 10 1 11 (Var (VarRef "x"))),([BlockComment ["B"]],at 1 16 1 17 (Var (VarRef "y")))])
        , example "newlines" "Just\n x\n y" $ at 1 1 3 3 (Data "Just" [([],at 2 2 2 3 (Var (VarRef "x"))),([],at 3 2 3 3 (Var (VarRef "y")))])
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
        [ example "" "(x,y)" $ at 1 1 1 6 (Tuple [Commented [] (at 1 2 1 3 (Var (VarRef "x"))) [],Commented [] (at 1 4 1 5 (Var (VarRef "y"))) []])
        , example "whitespace" "( x , y )" $ at 1 1 1 10 (Tuple [Commented [] (at 1 3 1 4 (Var (VarRef "x"))) [],Commented [] (at 1 7 1 8 (Var (VarRef "y"))) []])
        , example "comments" "({-A-}x{-B-},{-C-}y{-D-})" $ at 1 1 1 26 (Tuple [Commented [BlockComment ["A"]] (at 1 7 1 8 (Var (VarRef "x"))) [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (Var (VarRef "y"))) [BlockComment ["D"]]])
        , example "newlines" "(\n x\n ,\n y\n )" $ at 1 1 5 3 (Tuple [Commented [] (at 2 2 2 3 (Var (VarRef "x"))) [],Commented [] (at 4 2 4 3 (Var (VarRef "y"))) []])
        ]

    , testGroup "empty list pattern"
        [ example "" "[]" $ at 1 1 1 3 (EmptyListPattern [])
        , example "whitespace" "[ ]" $ at 1 1 1 4 (EmptyListPattern [])
        , example "comments" "[{-A-}]" $ at 1 1 1 8 (EmptyListPattern [BlockComment ["A"]])
        , example "newlines" "[\n ]" $ at 1 1 2 3 (EmptyListPattern [])
        ]

    , testGroup "list"
        [ example "" "[x,y]" $ at 1 1 1 6 (List [Commented [] (at 1 2 1 3 (Var (VarRef "x"))) [],Commented [] (at 1 4 1 5 (Var (VarRef "y"))) []])
        , example "single element" "[x]" $ at 1 1 1 4 (List [Commented [] (at 1 2 1 3 (Var (VarRef "x"))) []])
        , example "whitespace" "[ x , y ]" $ at 1 1 1 10 (List [Commented [] (at 1 3 1 4 (Var (VarRef "x"))) [],Commented [] (at 1 7 1 8 (Var (VarRef "y"))) []])
        , example "comments" "[{-A-}x{-B-},{-C-}y{-D-}]" $ at 1 1 1 26 (List [Commented [BlockComment ["A"]] (at 1 7 1 8 (Var (VarRef "x"))) [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (Var (VarRef "y"))) [BlockComment ["D"]]])
        , example "newlines" "[\n x\n ,\n y\n ]" $ at 1 1 5 3 (List [Commented [] (at 2 2 2 3 (Var (VarRef "x"))) [],Commented [] (at 4 2 4 3 (Var (VarRef "y"))) []])
        ]

    , testGroup "cons pattern"
        [ example "" "a::b::c" $ at 1 1 1 8 (ConsPattern (at 1 1 1 2 (Var (VarRef "a")),[]) [Commented [] (at 1 4 1 5 (Var (VarRef "b"))) []] ([],at 1 7 1 8 (Var (VarRef "c"))))
        , example "two patterns" "a::b" $ at 1 1 1 5 (ConsPattern (at 1 1 1 2 (Var (VarRef "a")),[]) [] ([],at 1 4 1 5 (Var (VarRef "b"))))
        , example "whitespace" "a :: b :: c" $ at 1 1 1 12 (ConsPattern (at 1 1 1 2 (Var (VarRef "a")),[]) [Commented [] (at 1 6 1 7 (Var (VarRef "b"))) []] ([],at 1 11 1 12 (Var (VarRef "c"))))
        , example "comments" "a{-A-}::{-B-}b{-C-}::{-D-}c" $ at 1 1 1 28 (ConsPattern (at 1 1 1 2 (Var (VarRef "a")),[BlockComment ["A"]]) [Commented [BlockComment ["B"]] (at 1 14 1 15 (Var (VarRef "b"))) [BlockComment ["C"]]] ([BlockComment ["D"]],at 1 27 1 28 (Var (VarRef "c"))))
        , example "newlines" "a\n ::\n b\n ::\n c" $ at 1 1 5 3 (ConsPattern (at 1 1 1 2 (Var (VarRef "a")),[]) [Commented [] (at 3 2 3 3 (Var (VarRef "b"))) []] ([],at 5 2 5 3 (Var (VarRef "c"))))
        ]

    , testGroup "record"
        [ example "" "{a,b}" $ at 1 1 1 6 (Record [Commented [] "a" [],Commented [] "b" []])
        , example "single element" "{a}" $ at 1 1 1 4 (Record [Commented [] "a" []])
        , example "whitespace" "{ a , b }" $ at 1 1 1 10 (Record [Commented [] "a" [],Commented [] "b" []])
        , example "comments" "{{-A-}a{-B-},{-C-}b{-D-}}" $ at 1 1 1 26 (Record [Commented [BlockComment ["A"]] "a" [BlockComment ["B"]],Commented [BlockComment ["C"]] "b" [BlockComment ["D"]]])
        , example "newlines" "{\n a\n ,\n b\n }" $ at 1 1 5 3 (Record [Commented [] "a" [],Commented [] "b" []])
        , testCase "must have at least one field" $
            assertParseFailure expr "{}"
        ]

    , testGroup "alias"
        [ example "" "_ as x" $ at 1 1 1 7 (Alias (at 1 1 1 2 Anything,[]) ([],"x"))
        , example "left side has whitespace" "A b as x" $ at 1 1 1 9 (Alias (at 1 1 1 4 (Data "A" [([], at 1 3 1 4 (Var (VarRef "b")))]),[]) ([],"x"))
        , example "left side ctor without whitespace" "A as x" $ at 1 1 1 7 (Alias (at 1 1 1 2 (Data "A" []),[]) ([],"x"))
        , example "comments" "_{-A-}as{-B-}x" $ at 1 1 1 15 (Alias (at 1 1 1 2 Anything,[BlockComment ["A"]]) ([BlockComment ["B"]],"x"))
        , example "newlines" "_\n as\n x" $ at 1 1 3 3 (Alias (at 1 1 1 2 Anything,[]) ([],"x"))
        , example "nested" "(_ as x)as y" $ at 1 1 1 13 (Alias (at 1 2 1 8 (Alias (at 1 2 1 3 Anything,[]) ([],"x")),[]) ([],"y"))
        , example "nested (whitespace)" "(_ as x) as y" $ at 1 1 1 14 (Alias (at 1 2 1 8 (Alias (at 1 2 1 3 Anything,[]) ([],"x")),[]) ([],"y"))
        , testCase "nesting required parentheses" $
            assertParseFailure expr "_ as x as y"
        ]
    ]
