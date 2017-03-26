module Parse.LiteralTest where

import Test.Tasty
import Test.Tasty.HUnit

import Box (render)
import Data.Text (unpack)
import ElmFormat.Render.Box (formatLiteral)
import Parse.Literal (literal)
import Parse.TestHelpers (assertParse, assertParseFailure)


example :: String -> String -> String -> TestTree
example name input expected =
    testCase name $
        assertParse (fmap (unpack . render . formatLiteral) literal) input expected


tests :: TestTree
tests =
    testGroup "Parse.Literal"
    [ testGroup "Int"
        [ example "" "99" "99\n"
        , example "negative" "-99" "-99\n"
        , example "hexadecimal" "0xfF" "0xFF\n"
        , testCase "hexadecimal must start with 0" $
            assertParseFailure literal "xFF"
        , testCase "hexadecimal, must contain digits" $
            assertParseFailure literal "0x"
        ]

    , testGroup "Float"
        [ example "" "0.1" "0.1\n"
        , example "negative" "-0.1" "-0.1\n"
        , example "exponent" "9e3" "9.0e3\n"
        , example "positive exponent" "9e+3" "9.0e3\n"
        , example "negative exponent" "9e-3" "9.0e-3\n"
        , example "capital exponent" "9E3" "9.0e3\n"
        , testCase "exponent must have exponent digits" $
            assertParseFailure literal "9E"
        , testCase "exponent must have digits" $
            assertParseFailure literal "e3"
        , example "exponent and decimal" "9.1e3" "9.1e3\n"
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
        [ example "" "\"hello\"" "\"hello\"\n"
        , example "empty" "\"\"" "\"\"\n"
        , example "escaped double quote" "\"\\\"\"" "\"\\\"\"\n"
        ]

    , testGroup "multiline String"
        [ example ""
            "\"\"\"hello\n\"\n\"\"\""
            "\"\"\"hello\n\"\n\"\"\"\n"
        ]

    , testGroup "Char"
        [ example "" "\'a\'" "\'a\'\n"
        , example "escaped single quote" "\'\\\'\'" "\'\\\'\'\n"
        , testCase "Char (must have one character)" $
            assertParseFailure literal "\'\'"
        , testCase "Char (must have only one character)" $
            assertParseFailure literal "\'ab\'"
        ]
    ]
