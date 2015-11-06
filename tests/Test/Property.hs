module Test.Property where

import Elm.Utils ((|>))

import Data.Char
import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified AST.Module
import qualified Data.Text.Lazy as LazyText
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified Test.Generators


assertStringToString :: String -> Assertion
assertStringToString source =
    let
        source' = LazyText.pack source

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
        result == (Right ast)


simpleAst =
    case Parse.toEither $ Parse.parse $ LazyText.pack "module Main (..) where\n\nfoo =\n    8\n" of
        Right ast -> ast


reportFailedAst ast =
    let
        rendering = Render.render ast |> LazyText.unpack
        result = Render.render ast |> Parse.parse |> show
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
        (\s -> counterexample (reportFailedAst s) (astToAst s))

    , testCase "simple round trip" $
        assertStringToString "module Main (..) where\n\nfoo =\n    8\n"
    ]
