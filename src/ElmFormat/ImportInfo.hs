module ElmFormat.ImportInfo (ImportInfo(..), fromModule, fromImports) where

import AST.V0_16
import Elm.Utils ((|>))

import qualified AST.Module
import qualified AST.Variable
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

data ImportInfo =
    ImportInfo
        { _exposed :: Dict.Map LowercaseIdentifier [UppercaseIdentifier]
        , _aliases :: Bimap.Bimap UppercaseIdentifier [UppercaseIdentifier]
        , _directImports :: Set.Set [UppercaseIdentifier]
        , _ambiguous :: Dict.Map UppercaseIdentifier [[UppercaseIdentifier]]
        }
    deriving Show


fromModule :: AST.Module.Module -> ImportInfo
fromModule modu =
    fromImports (fmap snd $ snd $ AST.Module.imports modu)


fromImports :: Dict.Map [UppercaseIdentifier] AST.Module.ImportMethod -> ImportInfo
fromImports imports =
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
            case Dict.lookup importName imports of
                Nothing -> mempty
                Just importMethod ->
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
                getAlias importMethod =
                    case AST.Module.alias importMethod of
                        Just (_, (_, alias)) ->
                            Just alias

                        Nothing -> Nothing

                liftMaybe :: (a, Maybe b) -> Maybe (a, b)
                liftMaybe (_, Nothing) = Nothing
                liftMaybe (a, Just b) = Just (a, b)
            in
            Dict.toList imports
                |> fmap (fmap getAlias)
                |> Maybe.mapMaybe liftMaybe
                |> fmap (\(a, b) -> (b, a))
                |> Bimap.fromList

        noAlias importMethod =
            case AST.Module.alias importMethod of
                Just _ -> False
                Nothing -> True

        directs =
            Set.union
                (Set.singleton [UppercaseIdentifier "Basics"])
                (Dict.keysSet $ Dict.filter noAlias imports)

        ambiguous = Dict.empty
    in
    ImportInfo exposed aliases directs ambiguous
