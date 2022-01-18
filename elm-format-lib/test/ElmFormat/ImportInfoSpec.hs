module ElmFormat.ImportInfoSpec where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Module (ImportMethod(..), DetailedListing(..))
import AST.Listing (Listing(..))
import Test.Hspec

import qualified Data.Map as Dict
import qualified Data.Set as Set
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.KnownContents as KnownContents

spec :: Spec
spec =
    describe "ElmFormat.ImportInfo" $
    let
        makeEntry (a, b, c) =
            ( fmap UppercaseIdentifier a
            , ImportMethod (fmap (C ([], []) . UppercaseIdentifier) b) (C ([], []) c)
            )

        buildImportInfo i =
            i
                |> fmap makeEntry
                |> Dict.fromList
                |> ImportInfo.fromImports mempty
    in do
    describe "_directImports" $
        let
            assertIncludes = assert "include" True
            assertExcludes = assert "exclude" False

            assert what expected name i =
                let
                    set =
                        buildImportInfo i
                            |> ImportInfo._directImports
                in
                if Set.member (fmap UppercaseIdentifier name) set == expected
                    then pure ()
                    else expectationFailure ("expected " ++ show set ++ " to " ++ what ++ ": " ++ show name)
        in do
        it "includes Basics" $
          []
            |> assertIncludes ["Basics"]
        it "includes normal imports" $
          [ (["A"], Nothing, ClosedListing) ]
            |> assertIncludes ["A"]
        it "includes normal deep imports" $
          [ (["A", "Deep"], Nothing, ClosedListing) ]
            |> assertIncludes ["A", "Deep"]
        it "excludes imports with aliases" $
          [ (["A"], Just "X", ClosedListing) ]
            |> assertExcludes ["A"]

        -- TODO: what if the alias is the same as the import name?
    describe "_exposed" $ do
        it "includes exposed values" $
            buildImportInfo [(["B"], Nothing, ExplicitListing (DetailedListing (Dict.singleton (LowercaseIdentifier "oldValue") (C ([], []) ())) mempty mempty) False )]
                |> ImportInfo._exposed
                |> Dict.lookup (VarName $ LowercaseIdentifier "oldValue")
                |> assertEqual "contains oldValue" (Just [UppercaseIdentifier "B"])
        it "includes Html.Attributes.style" $
            buildImportInfo [(["Html", "Attributes"], Nothing, OpenListing (C ([], []) ()))]
                |> ImportInfo._exposed
                |> Dict.lookup (VarName $ LowercaseIdentifier "style")
                |> assertEqual "contains style" (Just [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"])
        it "includes Basics" $
            buildImportInfo []
                |> ImportInfo._exposed
                |> Dict.lookup (VarName $ LowercaseIdentifier "identity")
                |> assertEqual "contains identity" (Just [UppercaseIdentifier "Basics"])
    describe "_exposedTypes" $ do
        it "includes exposed types" $
            buildImportInfo [(["B"], Nothing, ExplicitListing (DetailedListing mempty mempty (Dict.singleton (UppercaseIdentifier "OldType") (C ([], []) (C [] ClosedListing)))) False )]
                |> ImportInfo._exposed
                |> Dict.lookup (TypeName $ UppercaseIdentifier "OldType")
                |> assertEqual "contains OldType" (Just [UppercaseIdentifier "B"])
    describe "_unresolvedExposingAll" $ do
        it "includes modules without known content" $
            buildImportInfo [(["B"], Nothing, OpenListing (C ([], []) ()))]
                |> ImportInfo._unresolvedExposingAll
                |> Set.member [UppercaseIdentifier "B"]
                |> assertEqual "contains B" True
        it "does not include moduels with known content" $
            [(["B"], Nothing, OpenListing (C ([], []) ()))]
                |> fmap makeEntry
                |> Dict.fromList
                |> ImportInfo.fromImports (KnownContents.fromFunction $ const $ Just [VarName $ LowercaseIdentifier "b"])
                |> ImportInfo._unresolvedExposingAll
                |> Set.member [UppercaseIdentifier "B"]
                |> assertEqual "does not contain B" False


assertEqual :: (Show a, Eq a) => String -> a -> a -> Expectation
assertEqual description expected actual =
    actual `shouldBe` expected
