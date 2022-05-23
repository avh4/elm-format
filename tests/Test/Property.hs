module Test.Property where

import Prelude hiding ((>>))
import Elm.Utils ((|>), (>>))

import AST.V0_16
import AST.Structure
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.IO ()
import Reporting.Annotation (Located)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.Maybe as Maybe
import qualified ElmFormat.Parse as Parse
import qualified ElmFormat.Render.ByteStringBuilder as Render
import qualified ElmVersion
import qualified Test.Generators ()
import qualified Test.ElmSourceGenerators
import qualified Data.Indexed as I
import Data.Coapplicative (extract)
import Test.QuickCheck (Property, Testable, counterexample, forAll)


assertStringToString :: String -> Expectation
assertStringToString source =
    let
        source' = Text.pack source

        result =
            Parse.parse ElmVersion.Elm_0_19 (Text.encodeUtf8 source')
                |> Parse.toEither
                |> fmap (I.fold2 $ I.Fix . extract)
                |> fmap (Render.render ElmVersion.Elm_0_19)
                |> fmap (Lazy.toStrict . Lazy.decodeUtf8 . B.toLazyByteString)
    in
        result `shouldBe` Right source'


astToAst :: I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'ModuleNK -> Expectation
astToAst ast =
    let
        result =
            ast
                |> I.fold2 (I.Fix . extract)
                |> Render.render ElmVersion.Elm_0_19
                |> B.toLazyByteString
                |> LB.toStrict
                |> Parse.parse ElmVersion.Elm_0_19
                |> Parse.toEither
    in
        result `shouldBe` Right ast


simpleAst :: I.Fix2 Located (ASTNS [UppercaseIdentifier]) 'ModuleNK
simpleAst =
    case Parse.toEither $ Parse.parse ElmVersion.Elm_0_19 $ Text.encodeUtf8 "module Main exposing (foo)\n\n\nfoo =\n  8\n" of
        Right ast -> ast


reportFailedAst :: I.Fix (ASTNS [UppercaseIdentifier]) 'ModuleNK -> String
reportFailedAst ast =
    let
        rendering =
            Render.render ElmVersion.Elm_0_19 ast
                |> B.toLazyByteString
                |> Lazy.decodeUtf8
                |> Lazy.unpack

        result =
            Render.render ElmVersion.Elm_0_19 ast
                |> B.toLazyByteString
                |> LB.toStrict
                |> Parse.parse ElmVersion.Elm_0_19
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

withCounterexample :: Testable prop => (t -> String) -> (t -> prop) -> t -> Property
withCounterexample fn prop s =
    counterexample (fn s) $ prop s


propertyTests :: Spec
propertyTests =
    -- [ testCase "simple AST round trip" $
    --     astToAst simpleAst
    -- , testProperty "rendered AST should parse as equivalent AST"
    --     $ withCounterexample reportFailedAst astToAst

    describe "valid Elm files" $ do
        prop "should parse" $
            forAll Test.ElmSourceGenerators.elmModule $ withCounterexample id $
              Text.pack >> Text.encodeUtf8 >> Parse.parse ElmVersion.Elm_0_19 >> Parse.toMaybe >> Maybe.isJust

        -- testProperty "should parse to the same AST after formatting"
        --     $ forAll Test.ElmSourceGenerators.elmModule $ withCounterexample id
        --       $ Text.pack >> Parse.parse ElmVersion.Elm_0_19 >> Parse.toMaybe
        --         >> fmap astToAst
        --         >> Maybe.fromMaybe (assertFailure "failed to parse original")

        it "simple round trip" $
            assertStringToString "module Main exposing (foo)\n\n\nfoo =\n    8\n"
        it "simple round trip with comments" $
            assertStringToString "module Main exposing (foo)\n\n\nfoo =\n    ( {- A -} 3 {- B -}, {- C -} 4 {- D -} )\n"
        it "simple round trip with comments" $
            assertStringToString "module Main exposing (commentedLiterals)\n\n\ncommentedLiterals =\n    ( {- int -} 1, {- float -} 0.1, {- char -} \'c\', {- string -} \"str\", {- boolean -} True )\n"
