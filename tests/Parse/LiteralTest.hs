module Parse.LiteralTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

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


tests :: Test
tests =
    testGroup "Parse.Literal"
    [ testGroup "Int"
        [ example "" "99" $ IntNum 99 DecimalInt
        , example "negative" "-99" $ IntNum (-99) DecimalInt
        , example "hexadecimal" "0xfF" $ IntNum 255 HexadecimalInt
        , testCase "hexadecimal must start with 0" $
            assertFailure literal "xFF"
        , testCase "hexadecimal, must contain digits" $
            assertFailure literal "0x"
        ]

    , testGroup "Float"
        [ example "" "0.1" $ FloatNum 0.1
        , example "negative" "-0.1" $ FloatNum (-0.1)
        , example "exponent" "9e3" $ FloatNum 9000.0
        , example "positive exponent" "9e+3" $ FloatNum 9000.0
        , example "negative exponent" "9e-3" $ FloatNum 0.009
        , example "capital exponent" "9E3" $ FloatNum 9000.0
        , testCase "exponent must have exponent digits" $
            assertFailure literal "9E"
        , testCase "exponent must have digits" $
            assertFailure literal "e3"
        , example "exponent and decimal" "9.1e3" $ FloatNum 9100.0
        , testCase "exponent and decimal, must have decimal digits" $
            assertFailure literal "9.e3"
        , testCase "must have digits" $
            assertFailure literal "."
        , testCase "must start with a digit" $
            assertFailure literal ".1"
        , testCase "decimal, must have decimal digits" $
            assertFailure literal "99."
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
            assertFailure literal "\'\'"
        , testCase "Char (must have only one character)" $
            assertFailure literal "\'ab\'"
        ]
    ]
