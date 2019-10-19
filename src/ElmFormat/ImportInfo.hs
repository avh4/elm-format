module ElmFormat.ImportInfo (ImportInfo(..), fromModule, fromImports) where

import AST.V0_16
import AST.Variable (Listing(..))
import Data.Maybe (fromMaybe)
import Elm.Utils ((|>))

import qualified AST.Module
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

data ImportInfo =
    ImportInfo
        { _exposed :: Dict.Map LowercaseIdentifier [UppercaseIdentifier]
        , _exposedTypes :: Dict.Map UppercaseIdentifier [UppercaseIdentifier]
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
        knownModuleContents :: [UppercaseIdentifier] -> Maybe AST.Module.DetailedListing
        knownModuleContents moduleName =
            case (\(UppercaseIdentifier x) -> x) <$> moduleName of
                ["Html", "Attributes"] ->
                    Just $ AST.Module.DetailedListing
                        (Dict.fromList $ fmap (\x -> (LowercaseIdentifier x, Commented [] () [])) $
                            [ "style"
                            ]
                        )
                        mempty
                        mempty
                _ -> Nothing

        getExposedValues moduleName (AST.Module.ImportMethod _ (_, (_, listing))) =
            Dict.fromList $ fmap (flip (,) moduleName) $
            case listing of
                ClosedListing -> []
                OpenListing _ -> fromMaybe [] $ Dict.keys . AST.Module.values <$> knownModuleContents moduleName
                ExplicitListing details _ -> Dict.keys $ AST.Module.values details

        exposed =
            -- TODO: mark ambiguous names if multiple modules expose them
            Dict.foldlWithKey (\a k v -> Dict.union a $ getExposedValues k v) mempty imports

        exposedTypes =
            let
                step dict moduleName (AST.Module.ImportMethod _ (_, (_, listing))) =
                    case listing of
                        ExplicitListing (AST.Module.DetailedListing _ _ exposedTypes) _ ->
                            Dict.union dict
                                (Dict.fromList $ fmap (\typeName -> (typeName, moduleName)) $ Dict.keys exposedTypes)
                        OpenListing _ -> dict
                        ClosedListing -> dict
            in
            Dict.foldlWithKey step mempty imports

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
    ImportInfo exposed exposedTypes aliases directs ambiguous
