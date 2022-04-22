module BoxSpec where

import Elm.Utils ((|>))

import Test.Hspec
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text

import Box
import Data.Text (Text)


trim :: String -> String
trim text =
    text
        |> LazyText.pack
        |> LazyText.lines
        |> map LazyText.stripEnd
        |> LazyText.unlines
        |> LazyText.unpack


assertLineOutput :: String -> Line -> Expectation
assertLineOutput expected actual =
    assertOutput (expected ++ "\n") (line actual)


assertOutput :: String -> Box -> Expectation
assertOutput expected actual =
    expected `shouldBe` trim (Text.unpack $ render actual)


word :: Text -> Box
word =
    line . identifier


block :: Text -> Box
block text =
    stack1
        [ line $ w <> w
        , line $ w <> w
        ]
    where
        w = identifier text


spec :: Spec
spec = describe "Box" $ do
    it "keyword" $
        assertLineOutput "module" $ keyword "module"
    it "identifier" $
        assertLineOutput "sqrt" $ identifier "sqrt"
    it "punctuation" $
        assertLineOutput "::" $ punc "::"
    it "row" $
        assertLineOutput "ab" $ identifier "a" <> identifier "b"
    it "space" $
        assertLineOutput "a b" $ identifier "a" <> space <> identifier "b"

    it "stack1" $
        assertOutput "foo\nbar\n" $
            stack1
                [ word "foo"
                , word "bar"
                ]
    it "indent" $
        assertOutput "    a\n    b\n" $
            indent $ stack1
                [ word "a"
                , word "b"
                ]

    describe "prefix in front of block with indented lines" $ do
        it "when prefix is smaller than a TAB" $ do
            prefix (keyword ">>") $ stack1
                [ word "a"
                , indent $ word "b"
                ]
            `shouldOutput`
            [ ">>a"
            , "    b"
            ]

        it "when prefix is longer than a TAB" $ do
            prefix (keyword ">>>>>") $ stack1
                [ word "a"
                , indent $ word "b"
                ]
            `shouldOutput`
            [ ">>>>>a"
            , "        b"
            ]


shouldOutput :: Box -> [Text] -> Expectation
shouldOutput box expected =
    Box.render box
    `shouldBe` Text.unlines expected
