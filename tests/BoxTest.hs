module BoxTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Text.Lazy as LazyText

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
        trim $ render $ depr $ actual


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


tests :: Test
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
        assertOutput "    a\n    b\n" $
            indent $ stack1
                [ word "a"
                , word "b"
                ]
    , testCase "indent (with leading spaces)" $
        assertOutput "    a\n" $
            line $ row [ space, Tab, identifier "a" ]

    , testCase "elmApplication (single line)" $
        assertOutput "a b c\n" $
            elmApplication (word "a" )
                $ map word [ "b", "c" ]
    , testCase "elmApplication (multiline)" $
        assertOutput
            ( unlines
                [ "aa"
                , "aa"
                , "    bb"
                , "    bb"
                , "    c"
                ]
            ) $
            elmApplication
                ( block "a" )
                [ block "b"
                , line $ identifier "c"
                ]
    , testCase "elmGroup (empty)" $
        assertOutput "()\n" $
            elmGroup True "(" "," ")" False []
    , testCase "elmGroup (single item, single line)" $
        assertOutput "( foo )\n" $
            elmGroup True "(" "," ")" False [ word "foo" ]
    , testCase "elmGroup (single line)" $
        assertOutput "( foo, bar )\n" $
            elmGroup True "(" "," ")" False [ word "foo", word "bar" ]
    , testCase "elmGroup (single line, no spaces)" $
        assertOutput "(foo, bar)\n" $
            elmGroup False "(" "," ")" False [ word "foo", word "bar" ]
    , testCase "elmGroup (multiline)" $
        assertOutput "( aa\n  aa\n, b\n, cc\n  cc\n)\n" $
            elmGroup True "(" "," ")" False [ block "a", word "b", block "c" ]
    , testCase "elmGroup (forced multiline)" $
        assertOutput "( a\n, b\n, c\n)\n" $
            elmGroup True "(" "," ")" True [ word "a", word "b", word "c" ]
    ]
