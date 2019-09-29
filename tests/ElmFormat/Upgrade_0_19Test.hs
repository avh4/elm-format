module ElmFormat.Upgrade_0_19Test where

import Elm.Utils ((|>))

import AST.V0_16
import AST.Module (ImportMethod(..))
import AST.Variable (Listing(..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Dict
import qualified Data.Set as Set
import ElmFormat.Upgrade_0_19 (mergeUpgradeImports, MatchedNamespace(..))

tests :: TestTree
tests =
    testGroup "ElmFormat.Upgrade_0_19"
    [ testGroup "mergeUpgradeImports" $
        let
            assertImports name method = assert "include" name (Just method)
            assertNotImported name = assert "exclude" name Nothing

            assert what name method i =
                Dict.lookup (fmap UppercaseIdentifier name) i
                    |> assertEqual ("expected " ++ show i ++ " to " ++ what ++ ": " ++ show name) method
        in
        [ testCase "keeps import from original" $
            mergeUpgradeImports
                (Dict.singleton [UppercaseIdentifier "A"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing))))
                Dict.empty
                Set.empty
                (Dict.singleton (MatchedImport [UppercaseIdentifier "A"]) 1)
                |> assertImports ["A"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing)))
        , testCase "removes unused imports that were the subject of the upgrade" $
            mergeUpgradeImports
                (Dict.singleton [UppercaseIdentifier "A"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing))))
                Dict.empty
                (Set.singleton [UppercaseIdentifier "A"])
                Dict.empty
                |> assertNotImported ["A"]
        , testCase "does not remove unused imports from the original" $
            mergeUpgradeImports
                (Dict.singleton [UppercaseIdentifier "A"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing))))
                Dict.empty
                Set.empty
                Dict.empty
                |> assertImports ["A"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing)))
        , testCase "adds import from upgrade if it is used" $
            mergeUpgradeImports
                Dict.empty
                (Dict.singleton [UppercaseIdentifier "B"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing))))
                Set.empty
                (Dict.singleton (MatchedImport [UppercaseIdentifier "B"]) 1)
                |> assertImports ["B"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing)))
        , testCase "does not add import from upgrade if it is not used" $
            mergeUpgradeImports
                Dict.empty
                (Dict.singleton [UppercaseIdentifier "B"] (c "1", ImportMethod Nothing (c "2", (c "3", ClosedListing))))
                Set.empty
                Dict.empty
                |> assertNotImported ["B"]
        ]
    ]


c :: String -> Comments
c s = [LineComment s]
