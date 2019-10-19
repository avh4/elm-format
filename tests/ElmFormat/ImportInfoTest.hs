module ElmFormat.ImportInfoTest where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Module (ImportMethod(..), DetailedListing(..))
import AST.Variable (Listing(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Dict
import qualified Data.Set as Set
import qualified ElmFormat.ImportInfo as ImportInfo

tests :: TestTree
tests =
    testGroup "ElmFormat.ImportInfo" $
    let
        buildImportInfo i =
            i
                |> fmap (\(a, b, c) -> (fmap UppercaseIdentifier a, ImportMethod (fmap (\x -> ([], ([], UppercaseIdentifier x))) b) ([], ([], c))))
                |> Dict.fromList
                |> ImportInfo.fromImports
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
    , testGroup "_exposed" $
        [ testCase "includes exposed values" $
            buildImportInfo [(["B"], Nothing, ExplicitListing (DetailedListing (Dict.singleton (LowercaseIdentifier "oldValue") (Commented [] () [])) mempty mempty) False )]
                |> ImportInfo._exposed
                |> Dict.lookup (LowercaseIdentifier "oldValue")
                |> assertEqual "contains oldValue" (Just [UppercaseIdentifier "B"])
        , testCase "includes Html.Attributes.style" $
            buildImportInfo [(["Html", "Attributes"], Nothing, OpenListing (Commented [] () []))]
                |> ImportInfo._exposed
                |> Dict.lookup (LowercaseIdentifier "style")
                |> assertEqual "contains style" (Just [UppercaseIdentifier "Html", UppercaseIdentifier "Attributes"])
        ]
    , testGroup "_exposedTypes" $
        [ testCase "includes exposed types" $
            buildImportInfo [(["B"], Nothing, ExplicitListing (DetailedListing mempty mempty (Dict.singleton (UppercaseIdentifier "OldType") (Commented [] ([], ClosedListing) []))) False )]
                |> ImportInfo._exposedTypes
                |> Dict.lookup (UppercaseIdentifier "OldType")
                |> assertEqual "contains OldType" (Just [UppercaseIdentifier "B"])
        ]
    ]
