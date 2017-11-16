module Test.Property where

import Prelude hiding ((>>))
import Elm.Utils ((|>), (>>))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.IO ()

import Reporting.Annotation (stripRegion)

import qualified AST.Module
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.Text as Render
import qualified ElmVersion
import qualified Test.Generators ()
import qualified Test.ElmSourceGenerators
import qualified Defaults

assertStringToString :: String -> Assertion
assertStringToString source =
    let
        source' = Text.pack source

        result =
            Parse.parse source'
                |> Parse.toEither
                |> fmap (Render.render ElmVersion.Elm_0_17 Defaults.defaultTabSize)
    in
        assertEqual "" (Right source') result


astToAst :: AST.Module.Module -> Assertion
astToAst ast =
    let
        result =
            ast
                |> Render.render ElmVersion.Elm_0_17 Defaults.defaultTabSize
                |> Parse.parse
                |> Parse.toEither
    in
        assertEqual ""
          (Right $ stripRegion ast)
          (fmap stripRegion result)


simpleAst =
    case Parse.toEither $ Parse.parse $ Text.pack "module Main exposing (..)\n\n\nfoo =\n  8\n" of
        Right ast -> ast


reportFailedAst ast =
    let
        rendering = Render.render ElmVersion.Elm_0_17 Defaults.defaultTabSize ast |> Text.unpack
        result =
            Render.render ElmVersion.Elm_0_17 Defaults.defaultTabSize ast
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

withCounterexample fn prop =
  (\s -> counterexample (fn s) $ prop s)


propertyTests :: TestTree
propertyTests =
    testGroup "example test group"
    [ testCase "simple AST round trip" $
        astToAst simpleAst
    , testProperty "rendered AST should parse as equivalent AST"
        $ withCounterexample reportFailedAst astToAst

    , testGroup "valid Elm files"
        [ testProperty "should parse"
            $ forAll Test.ElmSourceGenerators.elmModule $ withCounterexample id
              $ Text.pack >> Parse.parse >> Parse.toMaybe >> Maybe.isJust

        , testProperty "should parse to the same AST after formatting"
            $ forAll Test.ElmSourceGenerators.elmModule $ withCounterexample id
              $ Text.pack >> Parse.parse >> Parse.toMaybe
                >> fmap astToAst
                >> Maybe.fromMaybe (assertFailure "failed to parse original")
        ]

    , testCase "simple round trip" $
        assertStringToString "module Main exposing (..)\n\n\nfoo =\n    8\n"
    , testCase "simple round trip with comments" $
        assertStringToString "module Main exposing (..)\n\n\nfoo =\n    ( {- A -} 3 {- B -}, {- C -} 4 {- D -} )\n"
    , testCase "simple round trip with comments" $
        assertStringToString "module Main exposing (..)\n\n\ncommentedLiterals =\n    ( {- int -} 1, {- float -} 0.1, {- char -} \'c\', {- string -} \"str\", {- boolean -} True )\n"
    ]
