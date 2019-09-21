module ElmFormat.ImportInfo (ImportInfo(..), fromModule) where

import AST.V0_16
import Elm.Utils ((|>))

import qualified AST.Module
import qualified AST.Variable
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe

data ImportInfo =
    ImportInfo
        { _exposed :: Dict.Map LowercaseIdentifier [UppercaseIdentifier]
        , _aliases :: Bimap.Bimap [UppercaseIdentifier] UppercaseIdentifier
        }
    deriving Show


fromModule :: AST.Module.Module -> ImportInfo
fromModule modu =
    let
        -- these are things we know will get exposed for certain modules when we see "exposing (..)"
        -- only things that are currently useful for Elm 0.19 upgrade are included
        knownModuleContents :: Dict.Map [UppercaseIdentifier] [LowercaseIdentifier]
        knownModuleContents =
            Dict.fromList $
                fmap (\(a,b) -> (fmap UppercaseIdentifier a, fmap LowercaseIdentifier b))
                [ (["Html", "Attributes"], ["style"])
                ]

        exposed =
            -- currently this only checks for Html.Attributes (needed for Elm 0.19 upgrade)
            let
                importName = (fmap UppercaseIdentifier ["Html", "Attributes"])
            in
            case Dict.lookup importName (snd $ AST.Module.imports modu) of
                Nothing -> mempty
                Just (_, importMethod) ->
                    case AST.Module.exposedVars importMethod of
                        (_, (_, AST.Variable.OpenListing _)) ->
                            -- import Html.Attributes [as ...] exposing (..)
                            Dict.lookup importName knownModuleContents
                                |> Maybe.fromMaybe []
                                |> fmap (\n -> (n, importName))
                                |> Dict.fromList

                        (_, (_, AST.Variable.ExplicitListing details _)) ->
                            -- import Html.Attributes [as ...] exposing (some, stuff)
                            AST.Module.values details
                                |> Dict.keys
                                |> fmap (\n -> (n, importName))
                                |> Dict.fromList

                        _ -> mempty

        aliases =
            let
                getAlias (_, importMethod) =
                    case AST.Module.alias importMethod of
                        Just (_, (_, alias)) ->
                            Just alias

                        Nothing -> Nothing

                liftMaybe :: (a, Maybe b) -> Maybe (a, b)
                liftMaybe (_, Nothing) = Nothing
                liftMaybe (a, Just b) = Just (a, b)
            in
            snd (AST.Module.imports modu)
                |> Dict.toList
                |> fmap (fmap getAlias)
                |> Maybe.mapMaybe liftMaybe
                |> Bimap.fromList
    in
    ImportInfo exposed aliases
