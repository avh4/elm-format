module Parse.HelpersSpec where

import Test.Hspec hiding (example)

import AST.V0_16
import Parse.Helpers (parens'')
import Parse.IParser
import Parse.ParsecAdapter (lower)

import Parse.TestHelpers


example :: (Show a, Eq a) => IParser a -> String -> String -> a -> SpecWith ()
example parser name input expected =
    it name $
        assertParse parser input expected


x :: IParser String
x = (\x -> [x]) <$> lower


spec :: Spec
spec = describe "Parse.Helpers" $ do
    describe "parens''" $ do
        example (parens'' x) "single term" "(x)" $ Right [C ([], []) "x"]
        example (parens'' x) "whitespace" "( x )" $ Right [C ([], []) "x"]
        example (parens'' x) "comments" "({-A-}x{-B-})" $ Right [C ([BlockComment ["A"]], [BlockComment ["B"]]) "x"]

        example (parens'' x) "multiple terms" "(a,b,c)" $ Right [C ([], []) "a", C ([], []) "b", C ([], []) "c"]
        example (parens'' x) "whitespace" "( a , b , c )" $ Right [C ([], []) "a", C ([], []) "b", C ([], []) "c"]

        example (parens'' x) "no terms" "()" $ Left []
        example (parens'' x) "whitespace" "( )" $ Left []
        example (parens'' x) "comments" "({-A-})" $ Left [BlockComment ["A"]]
