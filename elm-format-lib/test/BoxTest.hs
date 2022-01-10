module BoxTest where

import Elm.Utils ((|>))

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text

import Box
import Data.Text (Text)
import Test.Tasty.Hspec


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
        trim $ Text.unpack $ render actual


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


test_tests :: TestTree
test_tests =
    testGroup "BoxTest"
    [ testCase "keyword" $
        assertLineOutput "module" $ keyword "module"
    , testCase "identifier" $
        assertLineOutput "sqrt" $ identifier "sqrt"
    , testCase "punctuation" $
        assertLineOutput "::" $ punc "::"
    , testCase "row" $
        assertLineOutput "ab" $ identifier "a" <> identifier "b"
    , testCase "space" $
        assertLineOutput "a b" $ identifier "a" <> space <> identifier "b"

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
    ]


spec_spec :: Spec
spec_spec =
    describe "Box" $ do

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
