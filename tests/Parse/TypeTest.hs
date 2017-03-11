module Parse.TypeTest where

import Elm.Utils ((|>))

import Test.HUnit (Assertion, assertEqual)
import Test.Tasty
import Test.Tasty.HUnit

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


tests :: TestTree
tests =
    testGroup "Parse.Type"
    [ testGroup "tuple type"
        [ example "" "(a,b)" $ at 1 1 1 6 (TupleType [Commented [] (at 1 2 1 3 (TypeVariable $ LowercaseIdentifier "a")) [],Commented [] (at 1 4 1 5 (TypeVariable $ LowercaseIdentifier "b")) []])
        , example "whitespace" "( a , b )" $ at 1 1 1 10 (TupleType [Commented [] (at 1 3 1 4 (TypeVariable $ LowercaseIdentifier "a")) [],Commented [] (at 1 7 1 8 (TypeVariable $ LowercaseIdentifier "b")) []])
        , example "comments" "({-A-}a{-B-},{-C-}b{-D-})" $ at 1 1 1 26 (TupleType [Commented [BlockComment ["A"]] (at 1 7 1 8 (TypeVariable $ LowercaseIdentifier "a")) [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (TypeVariable $ LowercaseIdentifier "b")) [BlockComment ["D"]]])
        , example "newlines" "(\n a\n ,\n b\n )" $ at 1 1 5 3 (TupleType [Commented [] (at 2 2 2 3 (TypeVariable $ LowercaseIdentifier "a")) [],Commented [] (at 4 2 4 3 (TypeVariable $ LowercaseIdentifier "b")) []])
        ]

    , testGroup "record type"
        [ testGroup "empty"
            [ example "" "{}" $ at 1 1 1 3 (EmptyRecordType [])
            , example "whitespace" "{ }" $ at 1 1 1 4 (EmptyRecordType [])
            , example "comments" "{{-A-}}" $ at 1 1 1 8 (EmptyRecordType [BlockComment ["A"]])
            ]

        , example "" "{x:m,y:n}" $ at 1 1 1 10 (RecordType [(Commented [] (LowercaseIdentifier "x") [],Commented [] (at 1 4 1 5 (TypeVariable $ LowercaseIdentifier "m")) [],False),(Commented [] (LowercaseIdentifier "y") [],Commented [] (at 1 8 1 9 (TypeVariable $ LowercaseIdentifier "n")) [],False)] False)
        , example "whitespace" "{ x : m , y : n }" $ at 1 1 1 18 (RecordType [(Commented [] (LowercaseIdentifier "x") [],Commented [] (at 1 7 1 8 (TypeVariable $ LowercaseIdentifier "m")) [],False),(Commented [] (LowercaseIdentifier "y") [],Commented [] (at 1 15 1 16 (TypeVariable $ LowercaseIdentifier "n")) [],False)] False)
        , example "comments" "{{-A-}x{-B-}:{-C-}m{-D-},{-E-}y{-F-}:{-G-}n{-H-}}" $ at 1 1 1 50 (RecordType [(Commented [BlockComment ["A"]] (LowercaseIdentifier "x") [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (TypeVariable $ LowercaseIdentifier "m")) [BlockComment ["D"]],False),(Commented [BlockComment ["E"]] (LowercaseIdentifier "y") [BlockComment ["F"]],Commented [BlockComment ["G"]] (at 1 43 1 44 (TypeVariable $ LowercaseIdentifier "n")) [BlockComment ["H"]],False)] False)
        , example "single field with comments" "{{-A-}x{-B-}:{-C-}m{-D-}}" $ at 1 1 1 26 (RecordType [(Commented [BlockComment ["A"]] (LowercaseIdentifier "x") [BlockComment ["B"]],Commented [BlockComment ["C"]] (at 1 19 1 20 (TypeVariable $ LowercaseIdentifier "m")) [BlockComment ["D"]],False)] False)
        , example "newlines" "{\n x\n :\n m\n ,\n y\n :\n n\n }" $ at 1 1 9 3 (RecordType [(Commented [] (LowercaseIdentifier "x") [],Commented [] (at 4 2 4 3 (TypeVariable $ LowercaseIdentifier "m")) [],True),(Commented [] (LowercaseIdentifier "y") [],Commented [] (at 8 2 8 3 (TypeVariable $ LowercaseIdentifier "n")) [],True)] True)
        ]

    , testGroup "record extension type"
        [ example "" "{a|x:m,y:n}" $ at 1 1 1 12 (RecordExtensionType (Commented [] (LowercaseIdentifier "a") []) [(Commented [] (LowercaseIdentifier "x") [],Commented [] (at 1 6 1 7 (TypeVariable $ LowercaseIdentifier "m")) [],False),(Commented [] (LowercaseIdentifier "y") [],Commented [] (at 1 10 1 11 (TypeVariable $ LowercaseIdentifier "n")) [],False)] False)
        , example "single field" "{a|x:m}" $ at 1 1 1 8 (RecordExtensionType (Commented [] (LowercaseIdentifier "a") []) [(Commented [] (LowercaseIdentifier "x") [],Commented [] (at 1 6 1 7 (TypeVariable $ LowercaseIdentifier "m")) [],False)] False)
        , example "whitespace" "{ a | x : m , y : n }" $ at 1 1 1 22 (RecordExtensionType (Commented [] (LowercaseIdentifier "a") []) [(Commented [] (LowercaseIdentifier "x") [],Commented [] (at 1 11 1 12 (TypeVariable $ LowercaseIdentifier "m")) [],False),(Commented [] (LowercaseIdentifier "y") [],Commented [] (at 1 19 1 20 (TypeVariable $ LowercaseIdentifier "n")) [],False)] False)
        , example "comments" "{{-A-}a{-B-}|{-C-}x{-D-}:{-E-}m{-F-},{-G-}y{-H-}:{-I-}n{-J-}}" $ at 1 1 1 62 (RecordExtensionType (Commented [BlockComment ["A"]] (LowercaseIdentifier "a") [BlockComment ["B"]]) [(Commented [BlockComment ["C"]] (LowercaseIdentifier "x") [BlockComment ["D"]],Commented [BlockComment ["E"]] (at 1 31 1 32 (TypeVariable $ LowercaseIdentifier "m")) [BlockComment ["F"]],False),(Commented [BlockComment ["G"]] (LowercaseIdentifier "y") [BlockComment ["H"]],Commented [BlockComment ["I"]] (at 1 55 1 56 (TypeVariable $ LowercaseIdentifier "n")) [BlockComment ["J"]],False)] False)
        , example "newlines" "{\n a\n |\n x\n :\n m\n ,\n y\n :\n n\n }" $ at 1 1 11 3 (RecordExtensionType (Commented [] (LowercaseIdentifier "a") []) [(Commented [] (LowercaseIdentifier "x") [],Commented [] (at 6 2 6 3 (TypeVariable $ LowercaseIdentifier "m")) [],True),(Commented [] (LowercaseIdentifier "y") [],Commented [] (at 10 2 10 3 (TypeVariable $ LowercaseIdentifier "n")) [],True)] True)
        , testCase "only allows simple base" $
            assertParseFailure expr "{()|x:m}"
        , testCase "only allows simple base" $
            assertParseFailure expr "{{}|x:m}"
        , testCase "must have fields" $
            assertParseFailure expr "{a|}"
        ]
    ]
