module ElmFormat.ImportInfoTest where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Module (ImportMethod(..), DetailedListing(..))
import AST.Listing (Listing(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Dict
import qualified Data.Set as Set
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.KnownContents as KnownContents

test_tests :: TestTree
test_tests =
    testGroup "ElmFormat.ImportInfo" $
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
    in
    [ testGroup "_directImports" $
        let
            assertIncludes = assert "include" True
            assertExcludes = assert "exclude" False

            assert what expected name i =
                let
                    set =
                        buildImportInfo i
                            |> ImportInfo._directImports
                in
                Set.member (fmap UppercaseIdentifier name) set
                    |> assertEqual ("expected " ++ show set ++ " to " ++ what ++ ": " ++ show name) expected
        in
        [ testCase "includes Basics" $
          []
            |> assertIncludes ["Basics"]
        , testCase "includes normal imports" $
          [ (["A"], Nothing, ClosedListing) ]
            |> assertIncludes ["A"]
        , testCase "includes normal deep imports" $
          [ (["A", "Deep"], Nothing, ClosedListing) ]
            |> assertIncludes ["A", "Deep"]
        , testCase "excludes imports with aliases" $
          [ (["A"], Just "X", ClosedListing) ]
            |> assertExcludes ["A"]

        -- TODO: what if the alias is the same as the import name?
        ]
    , testGroup "_exposed"
        [ testCase "includes exposed values" $
            buildImportInfo [(["B"], Nothing, ExplicitListing (DetailedListing (Dict.singleton (LowercaseIdentifier "oldValue") (C ([], []) ())) mempty mempty) False )]
                |> ImportInfo._exposed
                |> Dict.lookup (VarName $ LowercaseIdentifier "oldValue")
                |> assertEqual "contains oldValue" (Just [UppercaseIdentifier "B"])
        , testCase "includes Html.Attributes.style" $
            buildImportInfo [(["Html", "Attributes"], Nothing, OpenListing (C ([], []) ()))]
                |> ImportInfo._exposed
                |> Dict.lookup (VarName $ LowercaseIdentifier "style")
                |> assertEqual "contains style" (Just [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"])
        , testCase "includes Basics" $
            buildImportInfo []
                |> ImportInfo._exposed
                |> Dict.lookup (VarName $ LowercaseIdentifier "identity")
                |> assertEqual "contains identity" (Just [UppercaseIdentifier "Basics"])
        ]
    , testGroup "_exposedTypes"
        [ testCase "includes exposed types" $
            buildImportInfo [(["B"], Nothing, ExplicitListing (DetailedListing mempty mempty (Dict.singleton (UppercaseIdentifier "OldType") (C ([], []) (C [] ClosedListing)))) False )]
                |> ImportInfo._exposed
                |> Dict.lookup (TypeName $ UppercaseIdentifier "OldType")
                |> assertEqual "contains OldType" (Just [UppercaseIdentifier "B"])
        ]
    , testGroup "_unresolvedExposingAll"
        [ testCase "includes modules without known content" $
            buildImportInfo [(["B"], Nothing, OpenListing (C ([], []) ()))]
                |> ImportInfo._unresolvedExposingAll
                |> Set.member [UppercaseIdentifier "B"]
                |> assertEqual "contains B" True
        , testCase "does not include moduels with known content" $
            [(["B"], Nothing, OpenListing (C ([], []) ()))]
                |> fmap makeEntry
                |> Dict.fromList
                |> ImportInfo.fromImports (KnownContents.fromFunction $ const $ Just [VarName $ LowercaseIdentifier "b"])
                |> ImportInfo._unresolvedExposingAll
                |> Set.member [UppercaseIdentifier "B"]
                |> assertEqual "does not contain B" False
        ]
    ]
