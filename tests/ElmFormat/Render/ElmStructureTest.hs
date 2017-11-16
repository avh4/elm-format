module ElmFormat.Render.ElmStructureTest where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text

import AST.V0_16
import Box
import ElmFormat.Render.ElmStructure
import Defaults


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
        trim $ Text.unpack $ render defaultTabSize $ actual


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
    testGroup "ElmFormat.Render.ElmStructure"
    [ testCase "application (single line)" $
        assertOutput "a b c\n" $
            application (FAJoinFirst JoinAll) (word "a" )
                $ map word [ "b", "c" ]
    , testCase "application (multiline)" $
        assertOutput
            ( unlines
                [ "aa"
                , "aa"
                , "    bb"
                , "    bb"
                , "    c"
                ]
            ) $
            application (FAJoinFirst JoinAll)
                ( block "a" )
                [ block "b"
                , line $ identifier "c"
                ]
    , testCase "group (empty)" $
        assertOutput "()\n" $
            group defaultTabSize True "(" "," ")" False []
    , testCase "group (single item, single line)" $
        assertOutput "( foo )\n" $
            group defaultTabSize True "(" "," ")" False [ word "foo" ]
    , testCase "group (single line)" $
        assertOutput "( foo, bar )\n" $
            group defaultTabSize True "(" "," ")" False [ word "foo", word "bar" ]
    , testCase "group (single line, no spaces)" $
        assertOutput "(foo, bar)\n" $
            group defaultTabSize False "(" "," ")" False [ word "foo", word "bar" ]
    , testCase "group (multiline)" $
        assertOutput "( aa\n  aa\n, b\n, cc\n  cc\n)\n" $
            group defaultTabSize True "(" "," ")" False [ block "a", word "b", block "c" ]
    , testCase "group (forced multiline)" $
        assertOutput "( a\n, b\n, c\n)\n" $
            group defaultTabSize True "(" "," ")" True [ word "a", word "b", word "c" ]
    ]
