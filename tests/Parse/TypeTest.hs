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
import ElmVersion
import ElmFormat.Render.Box (formatType)
import qualified Box
import qualified Data.Text as Text


pending = at 0 0 0 0 $ TupleType []


example :: String -> String -> String -> TestTree
example name input expected =
    testCase name $
        assertParse (fmap (Text.unpack . Box.render . formatType Elm_0_18) expr) input expected


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
            [ example "" "{}" "{}\n"
            , example "whitespace" "{ }" "{}\n"
            , example "comments" "{{-A-}}" "{{- A -}}\n"
            ]

        , example ""
            "{x:m,y:n}"
            "{ x : m, y : n }\n"
        , example "whitespace"
            "{ x : m , y : n }"
            "{ x : m, y : n }\n"
        , example "comments"
            "{{-A-}x{-B-}:{-C-}m{-D-},{-E-}y{-F-}:{-G-}n{-H-}}"
            "{ {- A -} x {- B -} : {- C -} m\n\
            \\n\
            \{- D -}\n\
            \, {- E -} y {- F -} : {- G -} n\n\
            \\n\
            \{- H -}\n\
            \}\n"
        , example "single field with comments"
            "{{-A-}x{-B-}:{-C-}m{-D-}}"
            "{ {- A -} x {- B -} : {- C -} m\n\
            \\n\
            \{- D -}\n\
            \}\n"
        , example "newlines"
            "{\n x\n :\n m\n ,\n y\n :\n n\n }"
            "{ x :\n\
            \    m\n\
            \, y :\n\
            \    n\n\
            \}\n"
        ]

    , testGroup "record extension type"
        [ example ""
            "{a|x:m,y:n}"
            "{ a | x : m, y : n }\n"
        , example "single field"
            "{a|x:m}"
            "{ a | x : m }\n"
        , example "whitespace"
            "{ a | x : m , y : n }"
            "{ a | x : m, y : n }\n"
        , example "comments"
            "{{-A-}a{-B-}|{-C-}x{-D-}:{-E-}m{-F-},{-G-}y{-H-}:{-I-}n{-J-}}"
            "{ {- A -} a {- B -}\n\
            \    | {- C -} x {- D -} : {- E -} m\n\
            \    \n\
            \    {- F -}\n\
            \    , {- G -} y {- H -} : {- I -} n\n\
            \    \n\
            \    {- J -}\n\
            \}\n"
        , example "newlines"
            "{\n a\n |\n x\n :\n m\n ,\n y\n :\n n\n }"
            "{ a\n\
            \    | x :\n\
            \        m\n\
            \    , y :\n\
            \        n\n\
            \}\n"
        , testCase "only allows simple base" $
            assertParseFailure expr "{()|x:m}"
        , testCase "only allows simple base" $
            assertParseFailure expr "{{}|x:m}"
        , example "no fields (elm-compiler does not allow this)"
            "{a|}"
            "{ a |   }\n"
        ]
    ]
