module Parse.TypeTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

import Parse.Type
import Text.ParserCombinators.Parsec.Combinator (eof)
import AST.V0_16
import AST.Type
import Reporting.Annotation hiding (map, at)
import Reporting.Region

import Parse.TestHelpers


pending = at 0 0 0 0 $ RTuple []


example name input expected =
    testCase name $
        assertParse expr input expected


tests :: Test
tests =
    testGroup "Parse.Type"
    [ testGroup "unit"
        [ example "" "()" $ at 1 1 1 3 (RTuple [])
        , example "whitespace" "( )" $ at 1 1 1 4 (RTuple [])
        , example "comments" "({-A-})" $ at 1 1 1 8 (RTuple [])
        , example "newlines" "(\n)" $ at 1 1 2 2 (RTuple [])
        ]

    , testGroup "type variable"
        [ example "lowercase" "a" $ at 1 1 1 2 (RVar "a")
        , example "uppercase" "Foo" $ at 1 1 1 4 (RVar "Foo")
        ]

    , testGroup "constructor"
        [ example "" "Foo a b" $ at 1 1 1 8 (RApp (at 1 1 1 4 (RVar "Foo")) [at 1 5 1 6 (RVar "a"),at 1 7 1 8 (RVar "b")])
        , example "comments" "Foo{-A-}a{-B-}b" $ at 1 1 1 16 (RApp (at 1 1 1 4 (RVar "Foo")) [at 1 9 1 10 (RVar "a"),at 1 15 1 16 (RVar "b")]) -- TODO: parse comments
        , example "newlines" "Foo\n a\n b" $ at 1 1 3 3 (RApp (at 1 1 1 4 (RVar "Foo")) [at 2 2 2 3 (RVar "a"),at 3 2 3 3 (RVar "b")])
        ]

    , testGroup "tuple constructor"
        [ example "single comma" "(,) a b" $ at 1 1 1 8 (RApp (at 1 1 1 4 (RTupleFunction 2)) [at 1 5 1 6 (RVar "a"),at 1 7 1 8 (RVar "b")])
        , example "multiple commas" "(,,,) a b c d" $ at 1 1 1 14 (RApp (at 1 1 1 6 (RTupleFunction 4)) [at 1 7 1 8 (RVar "a"),at 1 9 1 10 (RVar "b"),at 1 11 1 12 (RVar "c"),at 1 13 1 14 (RVar "d")])
        ]
    ]
