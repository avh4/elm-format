module Text.PrettyPrint.Avh4.BlockSpec where

import Test.Hspec

import Text.PrettyPrint.Avh4.Block
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as Lazy
import qualified Data.ByteString.Builder as B
import qualified Data.Text.Lazy.Encoding as Lazy


assertLineOutput :: Lazy.Text -> Line -> Expectation
assertLineOutput expected actual =
    assertOutput (expected <> "\n") (line actual)


assertOutput :: Lazy.Text -> Block -> Expectation
assertOutput expected actual =
    expected `shouldBe` Lazy.decodeUtf8 (B.toLazyByteString $ render actual)


word :: ByteString -> Block
word =
    line . identifierByteString


block :: ByteString -> Block
block text =
    stack'
        (line $ w <> w)
        (line $ w <> w)
    where
        w = identifierByteString text


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
            stack'
                (word "foo")
                (word "bar")
    it "indent" $
        assertOutput "    a\n    b\n" $
            indent $ stack'
                (word "a")
                (word "b")

    describe "prefix in front of block with indented lines" $ do
        it "when prefix is smaller than a TAB" $ do
            prefix 2 (keyword ">>") $ stack'
                (word "a")
                (indent $ word "b")
            `shouldOutput`
            [ ">>a"
            , "    b"
            ]

        it "when prefix is longer than a TAB" $ do
            prefix 5 (keyword ">>>>>") $ stack'
                (word "a")
                (indent $ word "b")
            `shouldOutput`
            [ ">>>>>a"
            , "        b"
            ]


shouldOutput :: Block -> [Lazy.Text] -> Expectation
shouldOutput block expected =
    Lazy.decodeUtf8 (B.toLazyByteString $ render block)
    `shouldBe` Lazy.unlines expected
