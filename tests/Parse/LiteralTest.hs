module Parse.LiteralTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Tasty
import Test.Tasty.HUnit

import Parse.Literal
import Text.ParserCombinators.Parsec.Combinator (eof)
import AST.V0_16
import Reporting.Annotation hiding (map, at)
import Reporting.Region

import Parse.TestHelpers


pending = at 0 0 0 0 $ IntNum 0 DecimalInt


example name input expected =
    testCase name $
        assertParse literal input expected


tests :: TestTree
tests =
    testGroup "Parse.Literal"
    [ testGroup "Int"
        [ example "" "99" $ IntNum 99 DecimalInt
        , example "negative" "-99" $ IntNum (-99) DecimalInt
        , example "hexadecimal" "0xfF" $ IntNum 255 HexadecimalInt
        , testCase "hexadecimal must start with 0" $
            assertParseFailure literal "xFF"
        , testCase "hexadecimal, must contain digits" $
            assertParseFailure literal "0x"
        ]

    , testGroup "Float"
        [ example "" "0.1" $ FloatNum 0.1 DecimalFloat
        , example "negative" "-0.1" $ FloatNum (-0.1) DecimalFloat
        , example "exponent" "9e3" $ FloatNum 9000.0 ExponentFloat
        , example "positive exponent" "9e+3" $ FloatNum 9000.0 ExponentFloat
        , example "negative exponent" "9e-3" $ FloatNum 0.009 ExponentFloat
        , example "capital exponent" "9E3" $ FloatNum 9000.0 ExponentFloat
        , testCase "exponent must have exponent digits" $
            assertParseFailure literal "9E"
        , testCase "exponent must have digits" $
            assertParseFailure literal "e3"
        , example "exponent and decimal" "9.1e3" $ FloatNum 9100.0 ExponentFloat
        , testCase "exponent and decimal, must have decimal digits" $
            assertParseFailure literal "9.e3"
        , testCase "must have digits" $
            assertParseFailure literal "."
        , testCase "must start with a digit" $
            assertParseFailure literal ".1"
        , testCase "decimal, must have decimal digits" $
            assertParseFailure literal "99."
        ]

    , testGroup "String"
        [ example "" "\"hello\"" $ Str "hello" False
        , example "empty" "\"\"" $ Str "" False
        , example "escaped double quote" "\"\\\"\"" $ Str "\"" False
        ]

    , testGroup "multiline String"
        [ example "" "\"\"\"hello\n\"\n\"\"\"" $ Str "hello\n\"\n" True
        ]

    , testGroup "Char"
        [ example "" "\'a\'" $ Chr 'a'
        , example "escaped single quote" "\'\\\'\'" $ Chr '\''
        , testCase "Char (must have one character)" $
            assertParseFailure literal "\'\'"
        , testCase "Char (must have only one character)" $
            assertParseFailure literal "\'ab\'"
        ]
    ]
