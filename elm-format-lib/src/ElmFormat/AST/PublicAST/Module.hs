{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module ElmFormat.AST.PublicAST.Module (Module(..), fromModule, toModule) where

import ElmFormat.AST.PublicAST.Core
import ElmFormat.AST.PublicAST.Comment
import ElmFormat.AST.PublicAST.Expression
import ElmFormat.AST.PublicAST.Type
import qualified AST.V0_16 as AST
import qualified AST.Module as AST
import qualified AST.Listing as AST
import Data.Map.Strict (Map)
import qualified Data.Maybe as Maybe
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified Data.Map.Strict as Map
import qualified Data.Indexed as I
import AST.MatchReferences (fromMatched, matchReferences)
import Data.Text (Text)
import qualified Data.Either as Either
import qualified Data.Text as Text
import Data.Maybe (fromMaybe)


data Module
    = Module
        { moduleName :: ModuleName
        , imports :: Map ModuleName Import
        , body :: List (MaybeF LocatedIfRequested TopLevelStructure)
        }

fromModule :: Config -> AST.Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK) -> Module
fromModule config = \case
    modu@(AST.Module _ maybeHeader _ (C _ imports) body) ->
        let
            header =
                Maybe.fromMaybe AST.defaultHeader maybeHeader

            (AST.Header _ (C _ name) _ _) = header

            importInfo =
                ImportInfo.fromModule mempty modu

            normalize =
                mapNs (fromMatched []) . matchReferences importInfo
        in
        Module
            (ModuleName name)
            (Map.mapWithKey (\m (C comments i) -> fromImportMethod m i) $ Map.mapKeys ModuleName imports)
            (fromTopLevelStructures config $ normalize body)

toModule :: Module -> AST.Module [UppercaseIdentifier] (ASTNS Identity [UppercaseIdentifier] 'TopLevelNK)
toModule (Module (ModuleName name) imports body) =
    -- TODO: remove this placeholder
    AST.Module
        []
        (Just $ AST.Header
            AST.Normal
            (C ([], []) name)
            Nothing
            Nothing
        )
        (noRegion Nothing)
        (C [] $ Map.mapKeys (\(ModuleName ns) -> ns) $ C [] . toImportMethod <$> imports)
        (f $ AST.TopLevel $ mconcat $ fmap (toTopLevelStructures . extract) body)
    where
        f = I.Fix . Identity

instance ToJSON Module where
    toJSON = undefined
    toEncoding = \case
        Module moduleName imports body ->
            pairs $ mconcat
                [ "moduleName" .= moduleName
                , "imports" .= imports
                , "body" .= body
                ]

instance FromJSON Module where
    parseJSON = withObject "Module" $ \obj ->
        (\moduleName makeImports -> Module moduleName (Map.mapWithKey (\importModuleName makeImport -> makeImport importModuleName) makeImports))
            <$> obj .: "moduleName"
            <*> obj .:? "imports" .!= mempty
            <*> obj .: "body"


data Import
    = Import
        { as :: ModuleName
        , exposing :: AST.Listing AST.DetailedListing
        }
    deriving (Generic)

fromImportMethod :: ModuleName -> AST.ImportMethod -> Import
fromImportMethod moduleName (AST.ImportMethod alias (C comments exposing)) =
    let
        as_ =
            case alias of
                Nothing -> moduleName
                Just (C c a) -> ModuleName [ a ]
    in
    Import as_ exposing

toImportMethod :: Import -> AST.ImportMethod
toImportMethod (Import alias exposing) =
    AST.ImportMethod
        (case alias of
            ModuleName [single] ->
                Just $ C ([], []) single
            _ ->
                Nothing
        )
        (C ([], []) exposing)

instance ToJSON Import where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON (ModuleName -> Import) where
    -- This results in a function that when given that actual name of the module, returns the Import
    parseJSON = withObject "Import" $ \obj ->
        (\makeAs exposing moduleName -> Import (makeAs moduleName) exposing)
            <$> (fmap const <$> obj .:? "as") .!= id
            <*> obj .:? "exposing" .!= AST.ClosedListing


data TopLevelStructure
    = DefinitionStructure Definition
    | TypeAlias
        { name_ta :: UppercaseIdentifier
        , parameters_ta :: List LowercaseIdentifier
        , type_ta :: LocatedIfRequested Type_
        }
    | CustomType
        { name_ct :: UppercaseIdentifier
        , parameters_ct :: List LowercaseIdentifier
        , variants :: List CustomTypeVariant
        }
    | Comment_tls Comment
    | TODO_TopLevelStructure String

fromTopLevelStructures :: Config -> ASTNS Located [UppercaseIdentifier] 'TopLevelNK -> List (MaybeF LocatedIfRequested TopLevelStructure)
fromTopLevelStructures config (I.Fix (A _ (AST.TopLevel decls))) =
    let
        toDefBuilder :: AST.TopLevelStructure
                     (ASTNS Located [UppercaseIdentifier] 'DeclarationNK) -> MaybeF LocatedIfRequested (DefinitionBuilder TopLevelStructure)
        toDefBuilder decl =
            case fmap I.unFix decl of
                AST.Entry (A region entry) ->
                    JustF $ fromLocated config $ A region $
                    case entry of
                        AST.Definition (I.Fix (A _ pat)) args preEquals expr ->
                            DefDef pat args expr

                        AST.TypeAnnotation name typ ->
                            DefAnnotation name typ

                        AST.TypeAlias c1 (C (c2, c3) (AST.NameWithArgs name args)) (C c4 t) ->

                            DefOpaque $ TypeAlias name (fmap (\(C c a) -> a) args) (fromRawAST config t)

                        AST.Datatype (C (c1, c2) (AST.NameWithArgs name args)) variants ->
                            DefOpaque $ CustomType
                                name
                                ((\(C c a) -> a) <$> args)
                                ((\(C c a) -> mkCustomTypeVariant config a) <$> AST.toCommentedList variants)

                        other ->
                            DefOpaque $ TODO_TopLevelStructure ("TODO: " ++ show other)

                AST.BodyComment comment ->
                    NothingF $ DefOpaque $ Comment_tls (mkComment comment)

                _ ->
                    NothingF $ DefOpaque $
                        TODO_TopLevelStructure ("TODO: " ++ show decl)
    in
    mkDefinitions config DefinitionStructure $ fmap toDefBuilder decls

toTopLevelStructures :: TopLevelStructure -> List (AST.TopLevelStructure (ASTNS Identity [UppercaseIdentifier] 'DeclarationNK))
toTopLevelStructures = \case
    DefinitionStructure (Definition name parameters Nothing expression) ->
        pure $ AST.Entry $ I.Fix $ Identity $ AST.Definition
            (I.Fix $ Identity $ AST.VarPattern name)
            (C [] . toRawAST . pattern <$> parameters)
            []
            (toRawAST expression)

    DefinitionStructure (Definition name [] (Just typ) expression) ->
        [ AST.Entry $ I.Fix $ Identity $ AST.TypeAnnotation
            (C [] $ VarRef () name)
            (C [] $ toRawAST typ)
        , AST.Entry $ I.Fix $ Identity $ AST.Definition
            (I.Fix $ Identity $ AST.VarPattern name)
            []
            []
            (toRawAST expression)
        ]

    DefinitionStructure (Definition name parameters (Just typ) expression) ->
        [ AST.Entry $ I.Fix $ Identity $ AST.TypeAnnotation
            (C [] $ VarRef () name)
            (C [] $ toRawAST $ LocatedIfRequested False $ noRegion $ FunctionType typ (fromMaybe (LocatedIfRequested False $ noRegion UnitType) . type_tp <$> parameters))
        , AST.Entry $ I.Fix $ Identity $ AST.Definition
            (I.Fix $ Identity $ AST.VarPattern name)
            (C [] . toRawAST . pattern <$> parameters)
            []
            (toRawAST expression)
        ]

    TypeAlias name parameters typ ->
        pure $ AST.Entry $ I.Fix $ Identity $ AST.TypeAlias
            []
            (C ([], []) (AST.NameWithArgs name (fmap (C []) parameters)))
            (C [] $ toRawAST typ)

    CustomType name parameters variants ->
        pure $ AST.Entry $ I.Fix $ Identity $ AST.Datatype
            (C ([], []) (AST.NameWithArgs name (fmap (C []) parameters)))
            (Either.fromRight undefined $ AST.fromCommentedList (C ([], [], Nothing) . fromCustomTypeVariant <$> variants))

    Comment_tls comment ->
        pure $ AST.BodyComment $ fromComment comment

instance ToJSON TopLevelStructure where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs TopLevelStructure where
    toPairs = \case
        DefinitionStructure def ->
            toPairs def

        TypeAlias name parameters t ->
            mconcat
                [ type_ "TypeAlias"
                , "name" .= name
                , "parameters" .= parameters
                , "type" .= t
                ]

        CustomType name parameters variants ->
            mconcat
                [ type_ "CustomType"
                , "name" .= name
                , "parameters" .= parameters
                , "variants" .= variants
                ]

        Comment_tls comment ->
            toPairs comment

        TODO_TopLevelStructure s ->
            "TODO" .= s

instance FromJSON TopLevelStructure where
    parseJSON = withObject "TopLevelStructure" $ \obj -> do
        tag :: Text <- obj .: "tag"
        case tag of
            "Definition" ->
                DefinitionStructure <$> parseJSON (Object obj)

            "TypeAlias" ->
                TypeAlias
                    <$> obj .: "name"
                    <*> obj .: "parameters"
                    <*> obj .: "type"

            "CustomType" ->
                CustomType
                    <$> obj .: "name"
                    <*> obj .: "parameters"
                    <*> obj .: "variants"

            "Comment" ->
                Comment_tls <$> parseJSON (Object obj)

            _ ->
                fail ("unexpected TopLevelStructure tag: " <> Text.unpack tag)
