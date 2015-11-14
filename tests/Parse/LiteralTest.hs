module Parse.LiteralTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

import Parse.Literal
import Text.ParserCombinators.Parsec.Combinator (eof)
import AST.V0_15
import AST.Literal
import Reporting.Annotation hiding (map, at)
import Reporting.Region

import Parse.TestHelpers


tests :: Test
tests =
    testGroup "Parse.Literal"
    [ testCase "Int" $
        assertParse literal "99" $ IntNum 99
    , testCase "Int (negative)" $
        assertParse literal "-99" $ IntNum (-99)
    , testCase "Int (hexadecimal)" $
        assertParse literal "0xfF" $ IntNum 255
    , testCase "Int (hexadecimal, must start with 0)" $
        assertFailure literal "xFF"
    , testCase "Int (hexadecimal, must contain digits)" $
        assertFailure literal "0x"

    , testCase "Float" $
        assertParse literal "0.1" $ FloatNum 0.1
    , testCase "Float (negative)" $
        assertParse literal "-0.1" $ FloatNum (-0.1)
    , testCase "Float (exponent)" $
        assertParse literal "9e3" $ FloatNum 9000.0
    , testCase "Float (positive exponent)" $
        assertParse literal "9e+3" $ FloatNum 9000.0
    , testCase "Float (negative exponent)" $
        assertParse literal "9e-3" $ FloatNum 0.009
    , testCase "Float (capital exponent)" $
        assertParse literal "9E3" $ FloatNum 9000.0
    , testCase "Float (exponent, must have exponent digits)" $
        assertFailure literal "9E"
    , testCase "Float (exponent, must have digits)" $
        assertFailure literal "e3"
    , testCase "Float (exponent and decimal)" $
        assertParse literal "9.1e3" $ FloatNum 9100.0
    , testCase "Float (exponent and decimal, must have decimal digits)" $
        assertFailure literal "9.e3"
    , testCase "Float (must have digits)" $
        assertFailure literal "."
    , testCase "Float (must start with a digit)" $
        assertFailure literal ".1"
    , testCase "Float (decimal, must have decimal digits)" $
        assertFailure literal "99."

    , testCase "String" $
        assertParse literal "\"hello\"" $ Str "hello" False
    , testCase "String (empty)" $
        assertParse literal "\"\"" $ Str "" False

    , testCase "String (multiline)" $
        assertParse literal "\"\"\"hello\n\"\n\"\"\"" $ Str "hello\n\"\n" True

    , testCase "Char" $
        assertParse literal "\'a\'" $ Chr 'a'
    , testCase "Char (must have one character)" $
        assertFailure literal "\'\'"
    , testCase "Char (must have only one character)" $
        assertFailure literal "\'ab\'"
    ]
