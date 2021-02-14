{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module AST.Json where

import Elm.Utils ((|>))

import AST.Listing
import AST.MatchReferences (fromMatched, matchReferences)
import AST.Module
import AST.Structure
import AST.V0_16
import Data.Coapplicative
import Data.Foldable
import Data.Maybe (mapMaybe)
import Reporting.Annotation
import Text.JSON hiding (showJSON)

import qualified Data.Indexed as I
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified ElmFormat.Version
import qualified Reporting.Region as Region
import Data.ReversedList (Reversed)
import qualified Data.ReversedList as ReversedList
import qualified ElmFormat.AST.PublicAST as PublicAST
import ElmFormat.AST.PublicAST (ToJSON(..), type_, ModuleName(..))


pleaseReport :: String -> String -> a
pleaseReport what details =
    error ("<elm-format-" ++ ElmFormat.Version.asString ++ ": "++ what ++ ": " ++ details ++ " -- please report this at https://github.com/avh4/elm-format/issues >")


showModule :: Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK) -> JSValue
showModule modu@(Module _ maybeHeader _ (C _ imports) body) =
    let
        c =
            PublicAST.Config True
    in
    showJSON c $ PublicAST.fromModule modu


recordJSON :: (ToJSON base, ToJSON value) => PublicAST.Config -> String -> String -> Maybe (C2 Before After base) -> Sequence (Pair LowercaseIdentifier value) -> JSValue
recordJSON c tag extensionTag base fields =
    let
        fieldsJSON =
            ( "fields"
            , makeObj $ fmap
                (\(Pair (C _ (LowercaseIdentifier key)) (C _ value) _) ->
                    (key, showJSON c value)
                )
                $ toList fields
            )

        fieldOrder =
            ( "fieldOrder"
            , JSArray $
                fmap (\(Pair (C _ key) _ _) -> showJSON c key) $
                toList fields
            )
    in
    case base of
        Just (C _ identifier) ->
            makeObj
                [ type_ extensionTag
                , ("base", showJSON c identifier)
                , fieldsJSON
                , ( "display"
                , makeObj
                    [ fieldOrder
                    ]
                )
                ]

        Nothing ->
            makeObj
                [ type_ tag
                , fieldsJSON
                , ( "display"
                , makeObj
                    [ fieldOrder
                    ]
                )
                ]


variableReference :: Region.Region -> String -> JSValue
variableReference region name =
    makeObj
        [ type_ "VariableReference"
        , ( "name" , JSString $ toJSString name )
        , sourceLocation region
        ]


sourceLocation :: Region.Region -> (String, JSValue)
sourceLocation region =
    ( "sourceLocation", showJSON undefined region )


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Located a
noRegion =
    at nowhere nowhere
