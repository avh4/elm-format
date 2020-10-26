module ElmFormat.ImportInfo (ImportInfo(..), fromModule, fromImports) where

import AST.V0_16
import AST.Listing (Listing(..), CommentedMap)
import Elm.Utils ((|>))

import AST.Module (Module, ImportMethod(..), DetailedListing(..))
import qualified AST.Module
import Data.Coapplicative
import qualified Data.Bimap as Bimap
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified ElmFormat.KnownContents as KnownContents
import ElmFormat.KnownContents (KnownContents)

data ImportInfo ns =
    ImportInfo
        { _exposed :: Dict.Map LocalName ns
        , _aliases :: Bimap.Bimap ns ns
        , _directImports :: Set.Set ns
        , _ambiguous :: Dict.Map LocalName [ns]
        , _unresolvedExposingAll :: Set.Set ns -- any modules with exposing(..) and we didn't know the module contents
        }
    deriving Show


fromModule ::
    KnownContents
    -> Module [UppercaseIdentifier] decl
    -> ImportInfo [UppercaseIdentifier]
fromModule knownContents modu =
    fromImports knownContents (fmap extract $ extract $ AST.Module.imports $ modu)


fromImports ::
    KnownContents
    -> Dict.Map [UppercaseIdentifier] ImportMethod
    -> ImportInfo [UppercaseIdentifier]
fromImports knownContents rawImports =
    let
        defaultImports :: Dict.Map [UppercaseIdentifier] ImportMethod
        defaultImports =
            Dict.fromList $
                fmap (\(m, i) -> (fmap UppercaseIdentifier m, ImportMethod Nothing (C ([], []) i)))
                [ ( [ "Basics" ], OpenListing (C ([], []) ()) )
                , ( [ "List" ], ClosedListing )
                , ( [ "Maybe" ]
                  , ExplicitListing
                      (DetailedListing mempty mempty $
                          Dict.fromList
                              [ ( UppercaseIdentifier "Maybe"
                                , C ([], []) $ C [] $
                                  ExplicitListing (Dict.fromList
                                                   [ (UppercaseIdentifier "Nothing", C ([], []) ())
                                                   , (UppercaseIdentifier "Just", C ([], []) ())
                                                   ]) False)]
                      )
                      False
                  )
                ]

        imports = Dict.union rawImports defaultImports -- NOTE: this MUST prefer rawImports when there is a duplicate key

        -- these are things we know will get exposed for certain modules when we see "exposing (..)"
        -- only things that are currently useful for Elm 0.19 upgrade are included
        moduleContents :: [UppercaseIdentifier] -> [LocalName]
        moduleContents moduleName =
            case (\(UppercaseIdentifier x) -> x) <$> moduleName of
                [ "Basics" ] ->
                    [ VarName $ LowercaseIdentifier "identity"
                    ]
                [ "Html", "Attributes" ] ->
                    [ VarName $ LowercaseIdentifier "style"
                    ]
                [ "List" ] ->
                    [ VarName $ LowercaseIdentifier "filterMap"
                    ]
                [ "Maybe" ] ->
                    [ CtorName $ UppercaseIdentifier "Nothing"
                    , CtorName $ UppercaseIdentifier "Just"
                    ]
                _ -> KnownContents.get moduleName knownContents |> Maybe.fromMaybe []

        getExposed moduleName (ImportMethod _ (C _ listing)) =
            Dict.fromList $ fmap (flip (,) moduleName) $
            case listing of
                ClosedListing -> []
                OpenListing _ ->
                    moduleContents moduleName
                ExplicitListing details _ ->
                    (fmap VarName $ Dict.keys $ AST.Module.values details)
                    <> (fmap TypeName $ Dict.keys $ AST.Module.types details)
                    <> (fmap CtorName $ foldMap (getCtorListings . extract . extract) $ Dict.elems $ AST.Module.types details)

        getCtorListings :: Listing (CommentedMap name ()) -> [name]
        getCtorListings = \case
            ClosedListing -> []
            OpenListing _ ->
                -- TODO: exposing (Type(..)) should pull in variant names from knownContents, though this should also be a warning because we can't know for sure which of those are for this type
                []
            ExplicitListing ctors _ -> Dict.keys ctors

        exposed =
            -- TODO: mark ambiguous names if multiple modules expose them
            Dict.foldlWithKey (\a k v -> Dict.union a $ getExposed k v) mempty imports

        aliases =
            let
                getAlias importMethod =
                    case AST.Module.alias importMethod of
                        Just (C _ alias) ->
                            Just [alias]

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

        exposesAll (ImportMethod _ (C _ listing)) =
            case listing of
                ExplicitListing _ _ -> False
                OpenListing _ -> True
                ClosedListing -> False

        unresolvedExposingAll =
            Dict.filter exposesAll rawImports
                |> Dict.keysSet
                |> Set.filter (not . KnownContents.isKnown knownContents)
    in
    ImportInfo exposed aliases directs ambiguous unresolvedExposingAll
