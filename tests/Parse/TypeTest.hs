module Parse.TypeTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Framework
import Test.Framework.Providers.HUnit

import Parse.Type
import Text.ParserCombinators.Parsec.Combinator (eof)
import AST.V0_16
import Reporting.Annotation hiding (map, at)
import Reporting.Region

import Parse.TestHelpers


pending = at 0 0 0 0 $ TupleType []


example name input expected =
    testCase name $
        assertParse expr input expected


tests :: Test
tests =
    testGroup "Parse.Type"
    [ testGroup "unit"
        [ example "" "()" $ at 1 1 1 3 (UnitType [])
        , example "whitespace" "( )" $ at 1 1 1 4 (UnitType [])
        , example "comments" "({-A-})" $ at 1 1 1 8 (UnitType [BlockComment ["A"]])
        , example "newlines" "(\n)" $ at 1 1 2 2 (UnitType [])
        ]

    , testGroup "type variable"
        [ example "lowercase" "a" $ at 1 1 1 2 (TypeVariable "a")
        ]

    , testGroup "constructor"
        [ example "" "Foo a b" $ at 1 1 1 8 (TypeConstruction (NamedConstructor "Foo") [([],at 1 5 1 6 (TypeVariable "a")),([],at 1 7 1 8 (TypeVariable "b"))])
        , example "no arguments" "Foo" $ at 1 1 1 4 (TypeConstruction (NamedConstructor "Foo") [])
        , example "comments" "Foo{-A-}a{-B-}b" $ at 1 1 1 16 (TypeConstruction (NamedConstructor "Foo") [([BlockComment ["A"]],at 1 9 1 10 (TypeVariable "a")),([BlockComment ["B"]],at 1 15 1 16 (TypeVariable "b"))])
        , example "newlines" "Foo\n a\n b" $ at 1 1 3 3 (TypeConstruction (NamedConstructor "Foo") [([],at 2 2 2 3 (TypeVariable "a")),([],at 3 2 3 3 (TypeVariable "b"))])
        ]

    , testGroup "tuple constructor"
        [ example "single comma" "(,) a b" $ at 1 1 1 8 (TypeConstruction (TupleConstructor 2) [([],at 1 5 1 6 (TypeVariable "a")),([],at 1 7 1 8 (TypeVariable "b"))])
        , example "multiple commas" "(,,,) a b c d" $ at 1 1 1 14 (TypeConstruction (TupleConstructor 4) [([],at 1 7 1 8 (TypeVariable "a")),([],at 1 9 1 10 (TypeVariable "b")),([],at 1 11 1 12 (TypeVariable "c")),([],at 1 13 1 14 (TypeVariable "d"))])
        ]

    , testGroup "parens"
        [ example "" "(a)" $ at 1 1 1 4 (TypeVariable "a")
        , example "whitespace" "( a )" $ at 1 1 1 6 (TypeVariable "a")
        , example "comments" "({-A-}a{-B-})" $ at 1 1 1 14 (TypeParens (Commented [BlockComment ["A"]] (at 1 7 1 8 (TypeVariable "a")) [BlockComment ["B"]]))
        , example "newlines" "(\n a\n )" $ at 1 1 3 3 (TypeVariable "a")
        ]

    , testGroup "tuple type"
        [ example "" "(a,b)" $ at 1 1 1 6 (TupleType [Commented [] (at 1 2 1 3 (TypeVariable "a")) [],Commented [] (at 1 4 1 5 (TypeVariable "b")) []])
        , example "whitespace" "( a , b )" $ at 1 1 1 10 (TupleType [Commented [] (at 1 3 1 4 (TypeVariable "a")) [],Commented [] (at 1 7 1 8 (TypeVariable "b")) []])
        , example "comments" "({-A-}a{-B-},{-C-}b{-D-})" $ at 1 1 1 26 (TupleType [Commented [BlockComment ["A"]] (at 1 7 1 8 (TypeVariable "a")) [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (TypeVariable "b")) [BlockComment ["D"]]])
        , example "newlines" "(\n a\n ,\n b\n )" $ at 1 1 5 3 (TupleType [Commented [] (at 2 2 2 3 (TypeVariable "a")) [],Commented [] (at 4 2 4 3 (TypeVariable "b")) []])
        ]

    , testGroup "record type"
        [ testGroup "empty"
            [ example "" "{}" $ at 1 1 1 3 (EmptyRecordType [])
            , example "whitespace" "{ }" $ at 1 1 1 4 (EmptyRecordType [])
            , example "comments" "{{-A-}}" $ at 1 1 1 8 (EmptyRecordType [BlockComment ["A"]])
            ]

        , example "" "{x:m,y:n}" $ at 1 1 1 10 (RecordType [(Commented [] "x" [],Commented [] (at 1 4 1 5 (TypeVariable "m")) [],False),(Commented [] "y" [],Commented [] (at 1 8 1 9 (TypeVariable "n")) [],False)] False)
        , example "whitespace" "{ x : m , y : n }" $ at 1 1 1 18 (RecordType [(Commented [] "x" [],Commented [] (at 1 7 1 8 (TypeVariable "m")) [],False),(Commented [] "y" [],Commented [] (at 1 15 1 16 (TypeVariable "n")) [],False)] False)
        , example "comments" "{{-A-}x{-B-}:{-C-}m{-D-},{-E-}y{-F-}:{-G-}n{-H-}}" $ at 1 1 1 50 (RecordType [(Commented [BlockComment ["A"]] "x" [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (TypeVariable "m")) [BlockComment ["D"]],False),(Commented [BlockComment ["E"]] "y" [BlockComment ["F"]],Commented [BlockComment ["G"]] (at 1 43 1 44 (TypeVariable "n")) [BlockComment ["H"]],False)] False)
        , example "single field with comments" "{{-A-}x{-B-}:{-C-}m{-D-}}" $ at 1 1 1 26 (RecordType [(Commented [BlockComment ["A"]] "x" [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (TypeVariable "m")) [BlockComment ["D"]],False)] False)
        , example "newlines" "{\n x\n :\n m\n ,\n y\n :\n n\n }" $ at 1 1 9 3 (RecordType [(Commented [] "x" [],Commented [] (at 4 2 4 3 (TypeVariable "m")) [],True),(Commented [] "y" [],Commented [] (at 8 2 8 3 (TypeVariable "n")) [],True)] True)
        ]

    , testGroup "record extension"
        [ example "" "{a|x:m,y:n}" $ at 1 1 1 12 (RecordExtensionType (Commented [] "a" []) [(Commented [] "x" [],Commented [] (at 1 6 1 7 (TypeVariable "m")) [],False),(Commented [] "y" [],Commented [] (at 1 10 1 11 (TypeVariable "n")) [],False)] False)
        , example "single field" "{a|x:m}" $ at 1 1 1 8 (RecordExtensionType (Commented [] "a" []) [(Commented [] "x" [],Commented [] (at 1 6 1 7 (TypeVariable "m")) [],False)] False)
        , example "whitespace" "{ a | x : m , y : n }" $ at 1 1 1 22 (RecordExtensionType (Commented [] "a" []) [(Commented [] "x" [],Commented [] (at 1 11 1 12 (TypeVariable "m")) [],False),(Commented [] "y" [],Commented [] (at 1 19 1 20 (TypeVariable "n")) [],False)] False)
        , example "comments" "{{-A-}a{-B-}|{-C-}x{-D-}:{-E-}m{-F-},{-G-}y{-H-}:{-I-}n{-J-}}" $ at 1 1 1 62 (RecordExtensionType (Commented [BlockComment ["A"]] "a" [BlockComment ["B"]]) [(Commented [BlockComment ["C"]] "x" [BlockComment ["D"]],Commented [BlockComment ["E"]] (at 1 31 1 32 (TypeVariable "m")) [BlockComment ["F"]],False),(Commented [BlockComment ["G"]] "y" [BlockComment ["H"]],Commented [BlockComment ["I"]] (at 1 55 1 56 (TypeVariable "n")) [BlockComment ["J"]],False)] False)
        , example "newlines" "{\n a\n |\n x\n :\n m\n ,\n y\n :\n n\n }" $ at 1 1 11 3 (RecordExtensionType (Commented [] "a" []) [(Commented [] "x" [],Commented [] (at 6 2 6 3 (TypeVariable "m")) [],True),(Commented [] "y" [],Commented [] (at 10 2 10 3 (TypeVariable "n")) [],True)] True)
        , testCase "only allows simple base" $
            assertFailure expr "{()|x:m}"
        , testCase "only allows simple base" $
            assertFailure expr "{{}|x:m}"
        , testCase "must have fields" $
            assertFailure expr "{a|}"
        ]
    ]
