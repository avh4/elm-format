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

    , testGroup "tuple type"
        [ example "" "(a,b)" $ at 1 1 1 6 (RTuple [at 1 2 1 3 (RVar "a"),at 1 4 1 5 (RVar "b")])
        , example "whitespace" "( a , b )" $ at 1 1 1 10 (RTuple [at 1 3 1 4 (RVar "a"),at 1 7 1 8 (RVar "b")])
        , example "comments" "({-A-}a{-B-},{-C-}b{-D-})" $ at 1 1 1 26 (RTuple [at 1 7 1 8 (RVar "a"),at 1 19 1 20 (RVar "b")])
        , example "newlines" "(\n a\n ,\n b\n )" $ at 1 1 5 3 (RTuple [at 2 2 2 3 (RVar "a"),at 4 2 4 3 (RVar "b")])
        ]

    , testGroup "record type"
        [ testGroup "empty"
            [ example "" "{}" $ at 1 1 1 3 (RRecord Nothing [] False)
            , example "whitespace" "{ }" $ at 1 1 1 4 (RRecord Nothing [] False)
            , example "comments" "{{-A-}}" $ at 1 1 1 8 (RRecord Nothing [] False)
            ]

        , example "" "{x:Int,y:Bool}" $ at 1 1 1 15 (RRecord Nothing [("x",at 1 4 1 7 (RVar "Int"),False),("y",at 1 10 1 14 (RVar "Bool"),False)] False)
        , example "single field" "{x:Int}" $ at 1 1 1 8 (RRecord Nothing [("x",at 1 4 1 7 (RVar "Int"),False)] False)
        , example "whitespace" "{ x : Int , y : Bool }" $ at 1 1 1 23 (RRecord Nothing [("x",at 1 7 1 10 (RVar "Int"),False),("y",at 1 17 1 21 (RVar "Bool"),False)] False)
        , example "comments" "{{-A-}x{-B-}:{-C-}Int{-D-},{-E-}y{-F-}:{-G-}Bool{-H-}}" $ at 1 1 1 55 (RRecord Nothing [("x",at 1 19 1 22 (RVar "Int"),False),("y",at 1 45 1 49 (RVar "Bool"),False)] False)
        , example "single field with comments" "{{-A-}x{-B-}:{-C-}Int{-D-}}" $ at 1 1 1 28 (RRecord Nothing [("x",at 1 19 1 22 (RVar "Int"),False)] False)
        , example "newlines" "{\n x\n :\n Int\n ,\n y\n :\n Bool\n }" $ at 1 1 9 3 (RRecord Nothing [("x",at 4 2 4 5 (RVar "Int"),True),("y",at 8 2 8 6 (RVar "Bool"),True)] True)
        ]

    , testGroup "record extension"
        [ example "" "{a|x:Int,y:Bool}" $ at 1 1 1 17 (RRecord (Just (at 1 2 1 3 (RVar "a"))) [("x",at 1 6 1 9 (RVar "Int"),False),("y",at 1 12 1 16 (RVar "Bool"),False)] False)
        , example "single field" "{a|x:Int}" $ at 1 1 1 10 (RRecord (Just (at 1 2 1 3 (RVar "a"))) [("x",at 1 6 1 9 (RVar "Int"),False)] False)
        , example "whitespace" "{ a | x : Int , y : Bool }" $ at 1 1 1 27 (RRecord (Just (at 1 3 1 4 (RVar "a"))) [("x",at 1 11 1 14 (RVar "Int"),False),("y",at 1 21 1 25 (RVar "Bool"),False)] False)
        , example "comments" "{{-A-}a{-B-}|{-C-}x{-D-}:{-E-}Int{-F-},{-G-}y{-H-}:{-I-}Bool{-J-}}" $ at 1 1 1 67 (RRecord (Just (at 1 7 1 8 (RVar "a"))) [("x",at 1 31 1 34 (RVar "Int"),False),("y",at 1 57 1 61 (RVar "Bool"),False)] False)
        , example "newlines" "{\n a\n |\n x\n :\n Int\n ,\n y\n :\n Bool\n }" $ at 1 1 11 3 (RRecord (Just (at 2 2 2 3 (RVar "a"))) [("x",at 6 2 6 5 (RVar "Int"),True),("y",at 10 2 10 6 (RVar "Bool"),True)] True)
        , testCase "only allows simple base" $
            assertFailure expr "{()|x:Int}"
        , testCase "only allows simple base" $
            assertFailure expr "{{}|x:Int}"
        , testCase "must have fields" $
            assertFailure expr "{a|}"
        ]
    ]
