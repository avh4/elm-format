module BoxTest where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text

import Box


trim :: String -> String
trim text =
    text
        |> LazyText.pack
        |> LazyText.lines
        |> map LazyText.stripEnd
        |> LazyText.unlines
        |> LazyText.unpack


assertLineOutput :: String -> Line -> Assertion
assertLineOutput expected actual =
    assertOutput (expected ++ "\n") (line actual)


assertOutput :: String -> Box -> Assertion
assertOutput expected actual =
    assertEqual expected expected $
        trim $ Text.unpack $ render $ actual


word :: String -> Box
word =
    line . identifier


block :: String -> Box
block text =
    stack1
        [ line $ row [ w, w ]
        , line $ row [ w, w ]
        ]
    where
        w = identifier text


tests :: TestTree
tests =
    testGroup "ElmFormat.Render.BoxTest"
    [ testCase "keyword" $
        assertLineOutput "module" $ keyword "module"
    , testCase "identifier" $
        assertLineOutput "sqrt" $ identifier "sqrt"
    , testCase "punctuation" $
        assertLineOutput "::" $ punc "::"
    , testCase "row" $
        assertLineOutput "ab" $ row [ identifier "a", identifier "b" ]
    , testCase "space" $
        assertLineOutput "a b" $ row [ identifier "a", space, identifier "b" ]

    , testCase "stack1" $
        assertOutput "foo\nbar\n" $
            stack1
                [ word "foo"
                , word "bar"
                ]
    , testCase "indent" $
        assertOutput "  a\n  b\n" $
            indent $ stack1
                [ word "a"
                , word "b"
                ]
    , testCase "indent (with leading spaces)" $
        assertOutput "  a\n" $
            prefix space $ indent $ line $ identifier "a"
    ]
