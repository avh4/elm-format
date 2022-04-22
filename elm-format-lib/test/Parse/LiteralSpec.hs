module Parse.LiteralSpec where

import Test.Hspec hiding (example)

import ElmFormat.Render.Box (formatLiteral)
import Parse.Literal (literal)
import Parse.TestHelpers (assertParse, assertParseFailure)

import qualified ElmVersion
import qualified Data.Text as Text
import qualified Box
import qualified Data.Fix as Fix
import qualified ElmFormat.Render.ElmStructure as ElmStructure


example :: String -> String -> String -> SpecWith ()
example name input expected =
    it name $
        assertParse (fmap (Text.unpack . Box.render . Fix.cata ElmStructure.render . formatLiteral ElmVersion.Elm_0_18) literal) input expected


spec :: Spec
spec = describe "Parse.Literal" $ do
    describe "Int" $ do
        example "" "99" "99\n"
        example "negative" "-99" "-99\n"
        describe "hexadecimal" $ do
            example "small" "0xfF" "0xFF\n"
            example "medium" "0xfF0" "0x0FF0\n"
            example "large" "0xfF000" "0x000FF000\n"
            example "huge" "0xfF0000000" "0x0000000FF0000000\n"
        it "hexadecimal must start with 0" $
            assertParseFailure literal "xFF"
        it "hexadecimal, must contain digits" $
            assertParseFailure literal "0x"

    describe "Float" $ do
        example "" "0.1" "0.1\n"
        example "negative" "-0.1" "-0.1\n"
        example "exponent" "9e3" "9.0e3\n"
        example "positive exponent" "9e+3" "9.0e3\n"
        example "negative exponent" "9e-3" "9.0e-3\n"
        example "capital exponent" "9E3" "9.0e3\n"
        it "exponent must have exponent digits" $
            assertParseFailure literal "9E"
        it "exponent must have digits" $
            assertParseFailure literal "e3"
        example "exponent and decimal" "9.1e3" "9.1e3\n"
        it "exponent and decimal, must have decimal digits" $
            assertParseFailure literal "9.e3"
        it "must have digits" $
            assertParseFailure literal "."
        it "must start with a digit" $
            assertParseFailure literal ".1"
        it "decimal, must have decimal digits" $
            assertParseFailure literal "99."

    describe "String" $ do
        example "" "\"hello\"" "\"hello\"\n"
        example "empty" "\"\"" "\"\"\n"
        example "escaped double quote" "\"\\\"\"" "\"\\\"\"\n"

    describe "multiline String" $ do
        example ""
            "\"\"\"hello\n\"\n\"\"\""
            "\"\"\"hello\n\"\n\"\"\"\n"

    describe "Char" $ do
        example "" "\'a\'" "\'a\'\n"
        example "escaped single quote" "\'\\\'\'" "\'\\\'\'\n"
        it "Char (must have one character)" $
            assertParseFailure literal "\'\'"
        it "Char (must have only one character)" $
            assertParseFailure literal "\'ab\'"
