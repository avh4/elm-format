module Test.Property where

import Elm.Utils ((|>))

import Data.Char
import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import AST.Module (stripRegion)

import qualified AST.Module
import qualified Data.Text as Text
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified Test.Generators


assertStringToString :: String -> Assertion
assertStringToString source =
    let
        source' = Text.pack source

        result =
            Parse.parse source'
                |> Parse.toEither
                |> fmap Render.render
    in
        assertEqual "" (Right source') result


astToAst :: AST.Module.Module -> Bool
astToAst ast =
    let
        result =
            ast
                |> Render.render
                |> Parse.parse
                |> Parse.toEither
    in
        (fmap stripRegion result) == (Right $ stripRegion ast)


simpleAst =
    case Parse.toEither $ Parse.parse $ Text.pack "module Main (..) where\n\n\nfoo =\n    8\n" of
        Right ast -> ast


reportFailedAst ast =
    let
        rendering = Render.render ast |> Text.unpack
        result =
            Render.render ast
                |> Parse.parse
                |> fmap stripRegion
                |> show
    in
        concat
            [ "=== Parsed as:\n"
            , result
            , "=== END OF parse\n"
            , "=== Rendering of failed AST:\n"
            , rendering
            , "=== END OF failed AST rendering\n"
            ]


propertyTests :: Test
propertyTests =
    testGroup "example test group"
    [ testCase "simple AST round trip" $
        assertBool "" (astToAst simpleAst)
    , testProperty "rendered AST should parse as equivalent AST"
        $ verbose (\s -> counterexample (reportFailedAst s) $ astToAst s)

    , testCase "simple round trip" $
        assertStringToString "module Main (..) where\n\n\nfoo =\n    8\n"
    , testCase "simple round trip with comments" $
        assertStringToString "module Main (..) where\n\n\nfoo =\n    ( {- A -} 3 {- B -}, {- C -} 4 {- D -} )\n"
    , testCase "simple round trip with comments" $
        assertStringToString "module Main (..) where\n\n\ncommentedLiterals =\n    ( {- int -} 1, {- float -} 0.1, {- char -} \'c\', {- string -} \"str\", {- boolean -} True )\n"
    ]
