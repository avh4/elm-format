{-# LANGUAGE DataKinds #-}
module AST.MatchReferencesTest (test_tests) where

import Elm.Utils ((|>))

import AST.V0_16
import AST.MatchReferences
import AST.Module (ImportMethod(..))
import AST.Structure
import Data.Functor.Identity
import qualified Data.Indexed as I
import Expect
import ElmFormat.ImportInfo (ImportInfo)
import qualified ElmFormat.KnownContents as KnownContents
import ElmVersion
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified Parse.Module
import qualified Parse.Parse as Parse
import qualified Reporting.Result as Result
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Dict
import Data.List.Split (splitOn)

test_tests :: TestTree
test_tests =
    testGroup "AST.MatchReferences"
    [ testGroup "matchReferences" $
        let
            test ::
                String
                -> [(String, List String)] -- knownContents
                -> List String -- imports
                -> List String -- locals
                -> Ref [String]
                -> Ref (MatchedNamespace [String])
                -> TestTree
            test name knownContents imports locals sourceAst' matchedAst' =
                let
                    sourceAst = fmap (fmap UppercaseIdentifier) sourceAst'
                    matchedAst = fmap (fmap $ fmap UppercaseIdentifier) matchedAst'
                    wrapExpr r =
                        case locals of
                            [] ->
                                -- no locals to define, so just make a var expression
                                I.Fix2 $ Identity $ VarExpr r
                            _ ->
                                -- define the provided locals in a let block
                                I.Fix2 $ Identity $
                                Let
                                    (fmap makeLetDeclaration locals)
                                    []
                                    (I.Fix2 $ Identity $ VarExpr r)
                in
                testCase name $
                    matchReferences (makeImportInfo knownContents imports) (wrapExpr sourceAst)
                        |> Expect.equals (wrapExpr matchedAst)
        in
        [ test "identifies unknown references"
            [] [] []
            (VarRef ["A"] (LowercaseIdentifier "a"))
            (VarRef (Unmatched ["A"]) (LowercaseIdentifier "a"))
        , test "matches references from an import"
            []
            [ "import A" ]
            []
            (VarRef ["A"] (LowercaseIdentifier "a"))
            (VarRef (MatchedImport True ["A"]) (LowercaseIdentifier "a"))
        , test "matches reference to a known value via exposing(..)"
            [ ("Html", ["div"]) ]
            [ "import Html exposing (..)" ]
            []
            (VarRef [] (LowercaseIdentifier "div"))
            (VarRef (MatchedImport False ["Html"]) (LowercaseIdentifier "div"))
        , test "determines references to local variables"
            [] []
            [ "a" ]
            (VarRef [] (LowercaseIdentifier "a"))
            (VarRef Local (LowercaseIdentifier "a"))
        , test "determines unqualified references that are unmatched"
            [] [] []
            (VarRef [] (LowercaseIdentifier "a"))
            (VarRef (UnmatchedUnqualified []) (LowercaseIdentifier "a"))
        , test "determines when an unqualified reference might match"
            []
            [ "import Test exposing (..)" ]
            []
            (VarRef [] (LowercaseIdentifier "describe"))
            (VarRef (UnmatchedUnqualified [["Test"]]) (LowercaseIdentifier "describe"))
        ]
    , testGroup "applyReferences" $
        let
            test ::
                String
                -> [(String, List String)] -- knownContents
                -> List String -- imports
                -> List String -- locals
                -> Ref (MatchedNamespace [String])
                -> Ref [String]
                -> TestTree
            test name knownContents imports locals sourceAst' matchedAst' =
                let
                    sourceAst = fmap (fmap $ fmap UppercaseIdentifier) sourceAst'
                    matchedAst = fmap (fmap UppercaseIdentifier) matchedAst'
                    wrapExpr r =
                        case locals of
                            [] ->
                                -- no locals to define, so just make a var expression
                                I.Fix2 $ Identity $ VarExpr r
                            _ ->
                                -- define the provided locals in a let block
                                I.Fix2 $ Identity $
                                Let
                                    (fmap makeLetDeclaration locals)
                                    []
                                    (I.Fix2 $ Identity $ VarExpr r)
                in
                testCase name $
                    applyReferences (makeImportInfo knownContents imports) (wrapExpr sourceAst)
                        |> Expect.equals (wrapExpr matchedAst)
        in
        [ test "local reference is unqualified"
            [] [] []
            (VarRef Local (LowercaseIdentifier "a"))
            (VarRef [] (LowercaseIdentifier "a"))
        , test "unmatched, unqualified reference is unqualified"
            [] [] []
            (VarRef (UnmatchedUnqualified []) (LowercaseIdentifier "a"))
            (VarRef [] (LowercaseIdentifier "a"))
        , test "unmatched, qualified reference is unchanged"
            [] [] []
            (VarRef (Unmatched ["XYZ", "ABC"]) (LowercaseIdentifier "a"))
            (VarRef ["XYZ", "ABC"] (LowercaseIdentifier "a"))
        , test "qualified, matched import becomes unqualified if explicitly exposed"
            []
            [ "import Html exposing (div)" ]
            []
            (VarRef (MatchedImport True ["Html"]) (LowercaseIdentifier "div"))
            (VarRef [] (LowercaseIdentifier "div"))
        , test "qualified, matched import remains qualified if not exposed"
            []
            [ "import Html" ]
            []
            (VarRef (MatchedImport True ["Html"]) (LowercaseIdentifier "div"))
            (VarRef ["Html"] (LowercaseIdentifier "div"))
        , test "qualified, matched import remains qualified if explicitly exposed but hidden by a local"
            []
            [ "import Html exposing (div)" ]
            [ "div" ]
            (VarRef (MatchedImport True ["Html"]) (LowercaseIdentifier "div"))
            (VarRef ["Html"] (LowercaseIdentifier "div"))
        , test "qualified, matched import remains qualified if explicitly exposed but there are exposing(..) with unknown content"
            []
            [ "import Html exposing (div)"
            , "import Html.Extra exposing (..)"
            ]
            []
            (VarRef (MatchedImport True ["Html"]) (LowercaseIdentifier "div"))
            (VarRef ["Html"] (LowercaseIdentifier "div"))
        , test "qualified, matched import becomes unqualified if explicitly exposed and there are exposing(..) with known content"
            [ ("Html.Extra", ["notDiv"]) ]
            [ "import Html exposing (div)"
            , "import Html.Extra exposing (..)"
            ]
            []
            (VarRef (MatchedImport True ["Html"]) (LowercaseIdentifier "div"))
            (VarRef [] (LowercaseIdentifier "div"))
        , test "unqualified, matched import remains unqualified if possible"
            []
            [ "import Html exposing (div)" ]
            []
            (VarRef (MatchedImport False ["Html"]) (LowercaseIdentifier "div"))
            (VarRef [] (LowercaseIdentifier "div"))
        , test "unqualified, matched import becomes qualified if obscured by a local"
            []
            [ "import Html exposing (div)" ]
            [ "div" ]
            (VarRef (MatchedImport False ["Html"]) (LowercaseIdentifier "div"))
            (VarRef ["Html"] (LowercaseIdentifier "div"))
        , test "unqualified, matched import becomes qualified if no longer exposed"
            []
            [ "import Html" ]
            []
            (VarRef (MatchedImport False ["Html"]) (LowercaseIdentifier "div"))
            (VarRef ["Html"] (LowercaseIdentifier "div"))
        ]
    ]


makeImportInfo :: [(String, List String)] -> [String] -> ImportInfo [UppercaseIdentifier]
makeImportInfo knownContentsRaw imports =
    let
        knownContents = knownContentsRaw |> fmap makeKnownContent |> Dict.fromList
    in
    ImportInfo.fromImports
        (KnownContents.fromFunction $ flip Dict.lookup knownContents)
        (imports |> fmap makeImportMethod |> Dict.fromList)


makeKnownContent :: (String, List String) -> (List UppercaseIdentifier, List LocalName)
makeKnownContent (moduleName, known) =
    ( UppercaseIdentifier <$> splitOn "." moduleName
    , VarName . LowercaseIdentifier <$> known
    )


makeImportMethod :: String -> ([UppercaseIdentifier], ImportMethod)
makeImportMethod importString =
    case Result.toMaybe $ Parse.parse importString (Parse.Module.import' Elm_0_19) of
        Nothing -> undefined -- Not handled: fix the test input to parse correctly
        Just (C _ moduleName, importMethod) ->
            (moduleName, importMethod)


makeLetDeclaration :: String -> I.Fix2 Identity (ASTNS ns) 'LetDeclarationNK
makeLetDeclaration name =
    I.Fix2 $ Identity $
    LetCommonDeclaration $ I.Fix2 $ Identity $ Definition
        (I.Fix2 $ Identity $ VarPattern $ LowercaseIdentifier name)
        [] []
        (I.Fix2 $ Identity $ Unit [])
