module Parse.HelpersTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

import AST.V0_16
import Parse.Helpers (IParser, parens'')
import Text.ParserCombinators.Parsec.Combinator (eof)
import Text.ParserCombinators.Parsec.Char (lower)
import Text.Parsec (string)

import Parse.TestHelpers


example parser name input expected =
    testCase name $
        assertParse parser input expected


x :: IParser String
x = (\x -> [x]) <$> lower


tests :: Test
tests =
    testGroup "Parse.Helpers"
    [ testGroup "parens''"
        [ example (parens'' x) "single term" "(x)" $ Right [([], "x", [])]
        , example (parens'' x) "whitespace" "( x )" $ Right [([], "x", [])]
        , example (parens'' x) "comments" "({-A-}x{-B-})" $ Right [([BlockComment ["A"]], "x", [BlockComment ["B"]])]

        , example (parens'' x) "multiple terms" "(a,b,c)" $ Right [([], "a", []), ([], "b", []), ([], "c", [])]
        , example (parens'' x) "whitespace" "( a , b , c )" $ Right [([], "a", []), ([], "b", []), ([], "c", [])]

        , example (parens'' x) "no terms" "()" $ Left []
        , example (parens'' x) "whitespace" "( )" $ Left []
        , example (parens'' x) "comments" "({-A-})" $ Left [BlockComment ["A"]]
        ]
    ]
