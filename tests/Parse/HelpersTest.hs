module Parse.HelpersTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Tasty
import Test.Tasty.HUnit

import AST.V0_16
import Parse.Helpers (parens'')
import Parse.IParser
import Text.ParserCombinators.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Char (lower)
import Text.Parsec (string)

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
        [ example (parens'' x) "single term" "(x)" $ Right [Commented [] "x" []]
        , example (parens'' x) "whitespace" "( x )" $ Right [Commented [] "x" []]
        , example (parens'' x) "comments" "({-A-}x{-B-})" $ Right [Commented [BlockComment ["A"]] "x" [BlockComment ["B"]]]

        , example (parens'' x) "multiple terms" "(a,b,c)" $ Right [Commented [] "a" [], Commented [] "b" [], Commented [] "c" []]
        , example (parens'' x) "whitespace" "( a , b , c )" $ Right [Commented [] "a" [], Commented [] "b" [], Commented [] "c" []]

        , example (parens'' x) "no terms" "()" $ Left []
        , example (parens'' x) "whitespace" "( )" $ Left []
        , example (parens'' x) "comments" "({-A-})" $ Left [BlockComment ["A"]]
        ]
    ]
