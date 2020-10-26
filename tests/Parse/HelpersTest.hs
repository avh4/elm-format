module Parse.HelpersTest where

import Test.Tasty
import Test.Tasty.HUnit

import AST.V0_16
import Parse.Helpers (parens'')
import Parse.IParser
import Text.ParserCombinators.Parsec.Char (lower)

import Parse.TestHelpers


example parser name input expected =
    testCase name $
        assertParse parser input expected


x :: IParser String
x = (\x -> [x]) <$> lower


tests :: TestTree
tests =
    testGroup "Parse.Helpers"
    [ testGroup "parens''"
        [ example (parens'' x) "single term" "(x)" $ Right [C ([], []) "x"]
        , example (parens'' x) "whitespace" "( x )" $ Right [C ([], []) "x"]
        , example (parens'' x) "comments" "({-A-}x{-B-})" $ Right [C ([BlockComment ["A"]], [BlockComment ["B"]]) "x"]

        , example (parens'' x) "multiple terms" "(a,b,c)" $ Right [C ([], []) "a", C ([], []) "b", C ([], []) "c"]
        , example (parens'' x) "whitespace" "( a , b , c )" $ Right [C ([], []) "a", C ([], []) "b", C ([], []) "c"]

        , example (parens'' x) "no terms" "()" $ Left []
        , example (parens'' x) "whitespace" "( )" $ Left []
        , example (parens'' x) "comments" "({-A-})" $ Left [BlockComment ["A"]]
        ]
    ]
