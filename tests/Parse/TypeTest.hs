module Parse.TypeTest where

import Test.Tasty
import Test.Tasty.HUnit

import Parse.Type
import AST.V0_16

import Parse.TestHelpers
import ElmVersion
import ElmFormat.Render.Box (formatType)
import qualified Box
import qualified Data.Text as Text


pending = at 0 0 0 0 $ TupleType []


example :: String -> String -> String -> TestTree
example name input expected =
    testCase name $
        assertParse (fmap (Text.unpack . Box.render . formatType Elm_0_19) (expr Elm_0_19)) input expected


tests :: TestTree
tests =
    testGroup "Parse.Type"
    [ testGroup "tuple type"
        [ example "" "(a,b)" "( a, b )\n"
        , example "whitespace" "( a , b )" "( a, b )\n"
        , example "comments"
            "({-A-}a{-B-},{-C-}b{-D-})"
            "( {- A -} a {- B -}, {- C -} b {- D -} )\n"
        , example "newlines" "(\n a\n ,\n b\n )" "( a, b )\n"
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
            \\n\
            \    {- F -}\n\
            \    , {- G -} y {- H -} : {- I -} n\n\
            \\n\
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
            assertParseFailure (expr Elm_0_19) "{()|x:m}"
        , testCase "only allows simple base" $
            assertParseFailure (expr Elm_0_19) "{{}|x:m}"
        , example "no fields (elm-compiler does not allow this)"
            "{a|}"
            "{ a |  }\n"
        ]
    ]
