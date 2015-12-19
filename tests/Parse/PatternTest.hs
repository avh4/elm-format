module Parse.PatternTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Text.Lazy as LazyText

import Parse.Pattern
import Parse.Helpers (IParser, iParse)
import AST.V0_15
import AST.Pattern
import AST.Literal
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


tests :: Test
tests =
    testGroup "Parse.Pattern"
    [ example "wildcard" "_" $ at 1 1 1 2 Anything

    , example "literal" "1" $ at 1 1 1 2 (Literal (IntNum 1))

    , example "variable" "a" $ at 1 1 1 2 (Var (VarRef "a"))

    , testGroup "data"
        [ example "" "Just _" $ at 1 1 1 7 (Data (VarRef "Just") [at 1 6 1 7 Anything])
        , example "comments" "Just{-A-}_" $ at 1 1 1 11 (Data (VarRef "Just") [at 1 10 1 11 Anything])
        , example "newlines" "Just\n _" $ at 1 1 2 3 (Data (VarRef "Just") [at 2 2 2 3 Anything])
        , mustBeIndented expr "Just\n _"

        , testGroup "infix constructor"
            [ example "" "_::_" $ at 1 1 1 5 (Data (OpRef "::") [at 1 1 1 2 Anything,at 1 4 1 5 Anything])
            , example "whitespace" "_ :: _" $ at 1 1 1 7 (Data (OpRef "::") [at 1 1 1 2 Anything,at 1 6 1 7 Anything])
            -- TODO: parse comments
            , example "comments" "_{-A-}::{-B-}_" $ at 1 1 1 15 (Data (OpRef "::") [at 1 1 1 2 Anything,at 1 14 1 15 Anything])
            , example "newlines" "_\n ::\n _" $ at 1 1 3 3 (Data (OpRef "::") [at 1 1 1 2 Anything,at 3 2 3 3 Anything])
            , mustBeIndented expr "_\n ::\n _"
            ]
        ]

    , testGroup "unit"
        [ example "" "()" $ at 1 1 1 3 (Tuple [])
        , example "whitespace" "( )" $ at 1 1 1 4 (Tuple [])
        -- TODO: parse comments
        , example "comments" "({-A-})" $ at 1 1 1 8 (Tuple [])
        , example "newlines" "(\n )" $ at 1 1 2 3 (Tuple [])
        , mustBeIndented expr "(\n )"
        ]

    , testGroup "parentheses"
        [ example "" "(_)" $ at 1 2 1 3 Anything
        , example "whitespace" "( _ )" $ at 1 3 1 4 Anything
        -- TODO: parse comments
        , example "comments" "({-A-}_{-B-})" $ at 1 7 1 8 Anything
        , example "newlines" "(\n _\n )" $ at 2 2 2 3 Anything
        , mustBeIndented expr "(\n _\n )"
        ]

    , testGroup "tuple"
        [ example "" "(_,_)" $ at 1 1 1 6 (Tuple [at 1 2 1 3 Anything,at 1 4 1 5 Anything])
        , example "whitespace" "( _ , _ )" $ at 1 1 1 10 (Tuple [at 1 3 1 4 Anything,at 1 7 1 8 Anything])
        -- TODO: parse comments
        , example "comments" "({-A-}_{-B-},{-C-}_{-D-})" $ at 1 1 1 26 (Tuple [at 1 7 1 8 Anything,at 1 19 1 20 Anything])
        , example "newlines" "(\n _\n ,\n _\n )" $ at 1 1 5 3 (Tuple [at 2 2 2 3 Anything,at 4 2 4 3 Anything])
        , mustBeIndented expr "(\n _\n ,\n _\n )"
        ]

    , testGroup "list"
        [ example "" "[_,_]" $ at 1 2 1 5 (List [at 1 2 1 3 Anything,at 1 4 1 5 Anything])
        , example "no elements" "[]" $ at 1 2 1 2 (List [])
        , example "single element" "[_]" $ at 1 2 1 3 (List [at 1 2 1 3 Anything])
        , example "whitespace" "[ _ , _ ]" $ at 1 3 1 8 (List [at 1 3 1 4 Anything,at 1 7 1 8 Anything])
        -- TODO: parse comments
        , example "comments" "[{-A-}_{-B-},{-C-}_{-D-}]" $ at 1 7 1 20 (List [at 1 7 1 8 Anything,at 1 19 1 20 Anything])
        , example "newlines" "[\n _\n ,\n _\n ]" $ at 2 2 4 3 (List [at 2 2 2 3 Anything,at 4 2 4 3 Anything])
        , mustBeIndented expr "[\n _\n ,\n _\n ]"
        ]

    , testGroup "record"
        [ example "" "{a,b}" $ at 1 1 1 6 (Record ["a","b"])
        , example "single element" "{a}" $ at 1 1 1 4 (Record ["a"])
        -- , example "empty record" "{}" $ pending
        , example "whitespace" "{ a , b }" $ at 1 1 1 10 (Record ["a","b"])
        -- TODO: parse comments
        , example "comments" "{{-A-}a{-B-},{-C-}b{-D-}}" $ at 1 1 1 26 (Record ["a","b"])
        , example "newlines" "{\n a\n ,\n b\n }" $ at 1 1 5 3 (Record ["a","b"])
        , mustBeIndented expr "{\n a\n ,\n b\n }"
        ]

    , testGroup "alias"
        [ example "" "_ as x" $ at 1 1 1 7 (Alias "x" (at 1 1 1 2 Anything))
        , example "left side has whitespace" "A b as x" $ at 1 1 1 9 (Alias "x" (at 1 1 1 4 (Data (VarRef "A") [at 1 3 1 4 (Var (VarRef "b"))])))
        , example "left side ctor without whitespace" "A as x" $ at 1 1 1 7 (Alias "x" (at 1 1 1 2 (Data (VarRef "A") [])))
        -- TODO: parse comments
        , example "comments" "_{-A-}as{-B-}x" $ at 1 1 1 15 (Alias "x" (at 1 1 1 2 Anything))
        , example "newlines" "_\n as\n x" $ at 1 1 3 3 (Alias "x" (at 1 1 1 2 Anything))
        , mustBeIndented expr "_\n as\n x"
        , example "nested" "(_ as x)as y" $ at 1 2 1 13 (Alias "y" (at 1 2 1 8 (Alias "x" (at 1 2 1 3 Anything))))
        , example "nested (whitespace)" "(_ as x) as y" $ at 1 2 1 14 (Alias "y" (at 1 2 1 8 (Alias "x" (at 1 2 1 3 Anything))))
        , testCase "nesting required parentheses" $
            assertFailure expr "_ as x as y"
        ]
    ]
