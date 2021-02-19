{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module ElmFormat.AST.PublicAST where

import ElmFormat.AST.Shared
import AST.V0_16 (NodeKind(..), Pair(..))
import qualified AST.V0_16 as AST
import qualified AST.Module as AST
import qualified AST.Listing as AST
import Reporting.Annotation (Located(A))
import Reporting.Region (Region)
import qualified Reporting.Region as Region
import qualified Data.List as List
import AST.Structure (ASTNS, ASTNS1, mapNs)
import Data.Indexed as I
import Data.Maybe (listToMaybe)
import Data.Coapplicative
import Data.ReversedList (Reversed)
import qualified Data.ReversedList as ReversedList
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified ElmFormat.ImportInfo as ImportInfo
import qualified Data.Maybe as Maybe
import Data.Maybe (mapMaybe)
import AST.MatchReferences (fromMatched, matchReferences)
import qualified Reporting.Annotation
import ElmFormat.AST.PatternMatching as PatternMatching
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Encoding.Internal (pair)
import qualified Data.Aeson.Encoding.Internal as AesonInternal
import GHC.Generics
import qualified Data.Char as Char


data LocatedIfRequested a
    = LocatedIfRequested Bool (Located a)
    deriving (Functor)

instance Coapplicative LocatedIfRequested where
    extract (LocatedIfRequested _ (A _ a)) = a

instance Prelude.Foldable LocatedIfRequested where
    foldMap f (LocatedIfRequested _ (A _ a)) = f a

instance Traversable LocatedIfRequested where
    traverse f (LocatedIfRequested b (A region a)) =
        fmap (LocatedIfRequested b . A region) $ f a

fromLocated :: Config -> Located a -> LocatedIfRequested a
fromLocated config =
    LocatedIfRequested (showSourceLocation config)

instance (ToPairs a, ToJSON a) => ToJSON (LocatedIfRequested a) where
    toJSON = undefined
    toEncoding (LocatedIfRequested showSourceLocation (A region a)) =
        if showSourceLocation
            then toEncoding (A region a)
            else toEncoding a

instance (FromJSON a) => FromJSON (LocatedIfRequested a) where
    parseJSON json =
        -- TODO: should refactor LocatedIfRequested to use Maybe Region instead of (Bool, Region)
        (LocatedIfRequested False . noRegion) <$> parseJSON json


data ModuleName =
    ModuleName [UppercaseIdentifier]
    deriving (Eq, Ord)

instance Show ModuleName where
    show (ModuleName ns) = List.intercalate "." $ fmap (\(UppercaseIdentifier v) -> v) ns

instance ToJSONKey ModuleName where
    toJSONKey =
        ToJSONKeyText
            (Text.pack . show)
            (AesonInternal.string . show)


data Comment
    = Comment
        { text :: String
        , display :: CommentDisplay
        }

mkComment :: AST.Comment -> Comment
mkComment = \case
    AST.BlockComment lines ->
        Comment
            (List.intercalate "\n" lines)
            (CommentDisplay BlockComment)

    AST.LineComment string ->
        Comment
            string
            (CommentDisplay BlockComment)

    -- | CommentTrickOpener
    -- | CommentTrickCloser
    -- | CommentTrickBlock String

instance ToJSON Comment where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Comment where
    toPairs = \case
        Comment text display ->
            mconcat
                [ type_ "Comment"
                , "text" .= text
                , "display" .= display
                ]

data CommentDisplay =
    CommentDisplay
        { commentType :: CommentType
        }
    deriving (Generic)

instance ToJSON CommentDisplay where
    toEncoding = genericToEncoding defaultOptions


data CommentType
    = BlockComment
    | LineComment
    deriving (Generic)

instance ToJSON CommentType where
    toEncoding = genericToEncoding defaultOptions


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
            (Map.mapWithKey (\m (C comments i) -> fromImportMethod m i) $ Map.mapKeys ModuleName $ imports)
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
        (C [] mempty)
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
    parseJSON = withObject "Module" $ \obj -> do
        moduleName <- obj .: "moduleName"
        -- TODO: parse imports
        -- TODO: parse body
        Module
            (ModuleName [ UppercaseIdentifier moduleName ])
            mempty
            <$> obj .: "body"


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

instance ToJSON Import where
    toEncoding = genericToEncoding defaultOptions


data TypedParameter
    = TypedParameter
        { pattern :: LocatedIfRequested Pattern
        , type_tp :: Maybe (LocatedIfRequested Type_)
        }

instance ToJSON TypedParameter where
    toJSON = undefined
    toEncoding = \case
        TypedParameter pattern typ ->
            pairs $ mconcat
                [ "pattern" .= pattern
                , "type" .= typ
                ]


data CustomTypeVariant
    = CustomTypeVariant
        { name :: UppercaseIdentifier
        , parameterTypes :: List (LocatedIfRequested Type_)
        }
    deriving (Generic)

instance ToJSON CustomTypeVariant where
    toEncoding = genericToEncoding defaultOptions

mkCustomTypeVariant :: Config -> AST.NameWithArgs UppercaseIdentifier (ASTNS Located [UppercaseIdentifier] 'TypeNK) -> CustomTypeVariant
mkCustomTypeVariant config (AST.NameWithArgs name args) =
    CustomTypeVariant
        name
        (fmap (\(C c a) -> fromRawAST config a) args)

data Definition
    = Definition
        { name_d :: LowercaseIdentifier
        , parameters_d :: List TypedParameter
        , returnType :: Maybe (LocatedIfRequested Type_)
        , expression :: LocatedIfRequested Expression
        }
    | TODO_Definition (List String)

mkDefinition ::
    Config
    -> ASTNS1 Located [UppercaseIdentifier] 'PatternNK
    -> List (AST.C1 'AST.BeforeTerm (ASTNS Located [UppercaseIdentifier] 'PatternNK))
    -> Maybe (AST.C2 'AST.BeforeSeparator 'AST.AfterSeparator (ASTNS Located [UppercaseIdentifier] 'TypeNK))
    -> ASTNS Located [UppercaseIdentifier] 'ExpressionNK
    -> Definition
mkDefinition config pat args annotation expr =
    case pat of
        AST.VarPattern name ->
            let
                (typedParams, returnType) =
                    maybe
                        ( fmap (\a -> ( a, Nothing ) ) args, Nothing )
                        ((\(a,b) -> ( fmap (fmap Just) a, Just b )) . PatternMatching.matchType args . (\(C (c1, c2) t) -> t))
                        annotation
            in
            Definition
                name
                (fmap (\(C c pat, typ) -> TypedParameter (fromRawAST config pat) (fmap (fromRawAST config) typ)) typedParams)
                (fmap (fromRawAST config) returnType)
                (fromRawAST config expr)

        _ ->
            TODO_Definition
                [ show pat
                , show args
                , show annotation
                , show expr
                ]

data DefinitionBuilder a
    = DefDef
        (ASTNS1 Located [UppercaseIdentifier] 'PatternNK)
        (List (AST.C1 'AST.BeforeTerm (ASTNS Located [UppercaseIdentifier] 'PatternNK)))
        (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
    | DefAnnotation
        (AST.C1 'AST.AfterTerm (AST.Ref ()))
        (AST.C1 'AST.BeforeTerm (ASTNS Located [UppercaseIdentifier] 'TypeNK))
    | DefOpaque a

mkDefinitions ::
    forall a.
    Config
    -> (Definition -> a)
    -> List (MaybeF LocatedIfRequested (DefinitionBuilder a))
    -> List (MaybeF LocatedIfRequested a)
mkDefinitions config fromDef items =
    let
        collectAnnotation :: DefinitionBuilder a -> Maybe (LowercaseIdentifier, AST.C2 'AST.BeforeSeparator 'AST.AfterSeparator (ASTNS Located [UppercaseIdentifier] 'TypeNK))
        collectAnnotation decl =
            case decl of
                DefAnnotation (C preColon (VarRef () name)) (C postColon typ) ->
                    Just (name, C (preColon, postColon) typ)
                _ -> Nothing

        annotations :: Map.Map LowercaseIdentifier (AST.C2 'AST.BeforeSeparator 'AST.AfterSeparator (ASTNS Located [UppercaseIdentifier] 'TypeNK))
        annotations =
            Map.fromList $ mapMaybe (collectAnnotation . extract) items

        merge :: DefinitionBuilder a -> Maybe a
        merge decl =
            case decl of
                DefDef pat args expr ->
                    let
                        annotation =
                            case pat of
                                AST.VarPattern name ->
                                    Map.lookup name annotations
                                _ -> Nothing
                    in
                    Just $ fromDef $ mkDefinition config pat args annotation expr

                DefAnnotation _ _ ->
                    -- TODO: retain annotations that don't have a matching definition
                    Nothing

                DefOpaque a ->
                    Just a
    in
    mapMaybe (sequenceA . fmap merge) items

instance ToJSON Definition where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Definition where
    toPairs = \case
        Definition name parameters returnType expression ->
            mconcat
                [ type_ "Definition"
                , "name" .= name
                , "parameters" .= parameters
                , "returnType" .= returnType
                , "expression" .= expression
                ]

        TODO_Definition info ->
            mconcat
                [ type_ "TODO: Definition"
                , "$" .= info
                ]

instance FromJSON Definition where
    parseJSON = withObject "Definition" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "Definition" ->
                Definition
                    <$> obj .: "name"
                    <*> return [] -- TODO
                    <*> return Nothing -- TODO
                    <*> obj .: "expression"

            _ ->
                fail ("unexpected Definition tag: " <> tag)


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
                     (ASTNS Located [UppercaseIdentifier] 'DeclarationNK) -> (MaybeF LocatedIfRequested (DefinitionBuilder TopLevelStructure))
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
                                (fmap (\(C c a) -> a) args)
                                (fmap (\(C c a) -> mkCustomTypeVariant config a) $ AST.toCommentedList variants)

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
    DefinitionStructure (Definition name parameters returnType expression) ->
        pure $ AST.Entry $ I.Fix $ Identity $ AST.Definition
            (I.Fix $ Identity $ AST.VarPattern name)
            [] -- TODO
            []
            (toRawAST expression)

    TypeAlias name parameters typ ->
        pure $ AST.Entry $ I.Fix $ Identity $ AST.TypeAlias
            []
            (C ([], []) (AST.NameWithArgs name (fmap (C []) parameters)))
            (C [] $ toRawAST typ)

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
        tag <- obj .: "tag"
        case tag of
            "Definition" ->
                DefinitionStructure <$> parseJSON (Object obj)

            "TypeAlias" ->
                TypeAlias
                    <$> obj .: "name"
                    <*> obj .: "parameters"
                    <*> obj .: "type"

            other ->
                fail ("unexpected TopLevelStructure tag: " <> tag)


data Reference
    = ExternalReference
        { module_ :: ModuleName
        , identifier :: Ref ()
        }
    | VariableReference
        { name :: Ref ()
        }

mkReference :: Ref [UppercaseIdentifier] -> Reference
mkReference = \case
    VarRef [] var ->
        VariableReference $ VarRef () var

    VarRef namespace var ->
        ExternalReference
            (ModuleName namespace)
            (VarRef () var)

    TagRef [] tag ->
        VariableReference $ TagRef () tag

    TagRef namespace tag ->
        ExternalReference
            (ModuleName namespace)
            (TagRef () tag)

    OpRef sym ->
        VariableReference $ OpRef sym

instance ToJSON Reference where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Reference where
    toPairs = \case
        ExternalReference module_ identifier ->
            mconcat
                [ type_ "ExternalReference"
                , "module" .= module_
                , "identifier" .= identifier
                ]

        VariableReference name ->
            mconcat
                [ type_ "VariableReference"
                , "name" .= name
                ]


data VariableDefinition
    = VariableDefinition
        { name :: LowercaseIdentifier
        }

instance ToJSON VariableDefinition where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs VariableDefinition where
    toPairs (VariableDefinition name) =
        mconcat
            [ type_ "VariableDefinition"
            , "name" .= name
            ]


data Pattern
    = AnythingPattern
    | UnitPattern
    | LiteralPattern LiteralValue
    | VariablePattern VariableDefinition
    | DataPattern
        { constructor :: Reference
        , arguments :: List (LocatedIfRequested Pattern) -- Non-empty
        }
    | TuplePattern
        { terms :: List (LocatedIfRequested Pattern) -- At least two items
        }
    | ListPattern -- Construct with mkListPattern
        { prefix :: List (LocatedIfRequested Pattern)
        , rest :: Maybe (LocatedIfRequested Pattern) -- Must not be a ListPattern
        }
    | RecordPattern
        { fields :: List VariableDefinition
        }
    | PatternAlias
        { alias :: VariableDefinition
        , pattern :: LocatedIfRequested Pattern
        }

mkListPattern :: List (LocatedIfRequested Pattern) -> Maybe (LocatedIfRequested Pattern) -> Pattern
mkListPattern prefix rest =
    case fmap extract rest of
        Just (ListPattern prefix2 rest2) ->
            ListPattern (prefix ++ prefix2) rest2

        _ ->
            ListPattern prefix rest

instance ToJSON Pattern where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Pattern where
    toPairs = \case
        AnythingPattern ->
            mconcat
                [ type_ "AnythingPattern"
                ]

        UnitPattern ->
            mconcat
                [ type_ "UnitPattern"
                ]

        LiteralPattern lit ->
            toPairs lit

        VariablePattern def ->
            toPairs def

        DataPattern constructor arguments ->
            mconcat
                [ type_ "DataPattern"
                , "constructor" .= constructor
                , "arguments" .= arguments
                ]

        TuplePattern terms ->
            mconcat
                [ type_ "TuplePattern"
                , "terms" .= terms
                ]

        ListPattern prefix rest ->
            mconcat
                [ type_ "ListPattern"
                , "prefix" .= prefix
                , "rest" .= rest
                ]

        RecordPattern fields ->
            mconcat
                [ type_ "RecordPattern"
                , "fields" .= fields
                ]

        PatternAlias alias pat ->
            mconcat
                [ type_ "PatternAlias"
                , "alias" .= alias
                , "pattern" .= pat
                ]


data Type_
    = UnitType
    | TypeReference
        { name_tr :: UppercaseIdentifier
        , module_ :: ModuleName
        , arguments :: List (LocatedIfRequested Type_)
        }
    | TypeVariable
        { name_tv :: LowercaseIdentifier
        }
    | TupleType
        { terms :: List (LocatedIfRequested Type_) -- At least two items
        }
    | RecordType
        { base :: Maybe LowercaseIdentifier
        , fields :: Map LowercaseIdentifier (LocatedIfRequested Type_) -- Cannot be empty if base is present
        , display :: RecordDisplay
        }
    | FunctionType
        { returnType :: LocatedIfRequested Type_
        , argumentTypes :: List (LocatedIfRequested Type_) -- Non-empty
        }

instance ToJSON Type_ where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Type_ where
    toPairs = \case
        UnitType ->
            mconcat
                [ type_ "UnitType"
                ]

        TypeReference name module_ arguments ->
            mconcat
                [ type_ "TypeReference"
                , "name" .= name
                , "module" .= module_
                , "arguments" .= arguments
                ]

        TypeVariable name ->
            mconcat
                [ type_ "TypeVariable"
                , "name" .= name
                ]

        TupleType terms ->
            mconcat
                [ type_ "TupleType"
                , "terms" .= terms
                ]

        RecordType Nothing fields display ->
            mconcat
                [ type_ "RecordType"
                , "fields" .= fields
                , "display" .= display
                ]

        RecordType (Just base) fields display ->
            mconcat
                [ type_ "RecordTypeExtension"
                , "base" .= base
                , "fields" .= fields
                , "display" .= display
                ]

        FunctionType returnType argumentTypes ->
            mconcat
                [ type_ "FunctionType"
                , "returnType" .= returnType
                , "argumentTypes" .= argumentTypes
                ]

instance FromJSON Type_ where
    parseJSON = withObject "Type" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "UnitType" ->
                return UnitType

            _ ->
                fail ("unexpected Type tag: " <> tag)


newtype RecordDisplay
    = RecordDisplay
        { fieldOrder :: List LowercaseIdentifier
        }
    deriving (Generic)

instance ToJSON RecordDisplay where
    toEncoding = genericToEncoding defaultOptions


data BinaryOperation
    = BinaryOperation
        { operator :: Reference
        , term :: LocatedIfRequested Expression
        }

instance ToJSON BinaryOperation where
    toJSON = undefined
    toEncoding = \case
        BinaryOperation operator term ->
            pairs $ mconcat
                [ "operator" .= operator
                , "term" .= term
                ]


data LetDeclaration
    = LetDefinition Definition
    | Comment_ld Comment

mkLetDeclarations :: Config -> List (ASTNS Located [UppercaseIdentifier] 'LetDeclarationNK) -> List (MaybeF LocatedIfRequested LetDeclaration)
mkLetDeclarations config decls =
    let
        toDefBuilder :: ASTNS1 Located [UppercaseIdentifier] 'LetDeclarationNK -> DefinitionBuilder LetDeclaration
        toDefBuilder = \case
            AST.LetDefinition (I.Fix (A _ pat)) args preEquals expr ->
                DefDef pat args expr

            AST.LetAnnotation name typ ->
                DefAnnotation name typ

            AST.LetComment comment ->
                DefOpaque $ Comment_ld (mkComment comment)
    in
    mkDefinitions config LetDefinition $ fmap (JustF . fmap toDefBuilder . fromLocated config . I.unFix) decls

instance ToJSON LetDeclaration where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs LetDeclaration where
    toPairs = \case
        LetDefinition def ->
            toPairs def

        Comment_ld comment ->
            toPairs comment


data CaseBranch
    = CaseBranch
        { pattern :: LocatedIfRequested Pattern
        , body :: LocatedIfRequested Expression
        }

instance ToPublicAST 'CaseBranchNK where
    type PublicAST 'CaseBranchNK = CaseBranch

    fromRawAST' config = \case
        AST.CaseBranch c1 c2 c3 pat body ->
            CaseBranch
                (fromRawAST config pat)
                (fromRawAST config body)

instance ToPairs CaseBranch where
    toPairs = \case
        CaseBranch pattern body ->
            mconcat
                [ "pattern" .= pattern
                , "body" .= body
                ]

instance ToJSON CaseBranch where
    toJSON = undefined
    toEncoding = pairs . toPairs


data Expression
    = UnitLiteral
    | LiteralExpression LiteralValue
    | VariableReferenceExpression Reference
    | FunctionApplication
        { function :: MaybeF LocatedIfRequested Expression
        , arguments :: List (LocatedIfRequested Expression)
        , display_fa :: FunctionApplicationDisplay
        }
    | BinaryOperatorList
        { first :: LocatedIfRequested Expression
        , operations :: List BinaryOperation
        }
    | UnaryOperator
        { operator :: AST.UnaryOperator
        , term :: LocatedIfRequested Expression
        }
    | ListLiteral
        { terms :: List (LocatedIfRequested Expression)
        }
    | TupleLiteral
        { terms :: List (LocatedIfRequested Expression) -- At least two items
        }
    | RecordLiteral
        { base :: Maybe LowercaseIdentifier
        , fields :: Map LowercaseIdentifier (LocatedIfRequested Expression) -- Cannot be empty if base is present
        , display_rl :: RecordDisplay
        }
    | RecordAccessFunction
        { field :: LowercaseIdentifier
        }
    | AnonymousFunction
        { parameters :: List (LocatedIfRequested Pattern) -- Non-empty
        , body :: LocatedIfRequested Expression
        }
    | IfExpression
        { if_ :: LocatedIfRequested Expression
        , then_ :: LocatedIfRequested Expression
        , else_ :: MaybeF LocatedIfRequested Expression
        }
    | LetExpression
        { declarations :: List (MaybeF LocatedIfRequested LetDeclaration)
        , body :: LocatedIfRequested Expression
        }
    | CaseExpression
        { subject :: LocatedIfRequested Expression
        , branches :: List (LocatedIfRequested CaseBranch)
        }
    | GLShader
        { shaderSource :: String
        }


data FunctionApplicationDisplay
    = FunctionApplicationDisplay
        { showAsRecordAccess :: Bool
        }

instance ToMaybeJSON FunctionApplicationDisplay where
    toMaybeEncoding = \case
        FunctionApplicationDisplay showAsRecordAccess ->
            case
                Maybe.catMaybes
                    [ if showAsRecordAccess
                        then Just ("showAsRecordAccess" .= True)
                        else Nothing
                    ]
            of
                [] -> Nothing
                some -> Just $ pairs $ mconcat some


data MaybeF f a
    = JustF (f a)
    | NothingF a
    deriving (Functor)

instance Prelude.Foldable f => Prelude.Foldable (MaybeF f) where
    foldMap f (JustF fa) = Prelude.foldMap f fa
    foldMap f (NothingF a) = f a

instance Traversable f => Traversable (MaybeF f) where
    traverse f (JustF fa) = fmap JustF $ sequenceA $ fmap f fa
    traverse f (NothingF a) = fmap NothingF $ f a

instance Coapplicative f => Coapplicative (MaybeF f) where
    extract (JustF fa) = extract fa
    extract (NothingF a) = a

instance (ToJSON a, ToJSON (f a)) => ToJSON (MaybeF f a) where
    toJSON = undefined
    toEncoding = \case
        JustF fa -> toEncoding fa
        NothingF a -> toEncoding a

instance (FromJSON (f a)) => FromJSON (MaybeF f a) where
    parseJSON json =
        -- TODO: should this fall back to parsing an `a`?
        JustF <$> parseJSON json


instance ToPublicAST 'ExpressionNK where
    type PublicAST 'ExpressionNK = Expression

    fromRawAST' config = \case
        AST.Unit comments ->
            UnitLiteral

        AST.Literal lit ->
            LiteralExpression lit

        AST.VarExpr var ->
            VariableReferenceExpression $ mkReference var

        AST.App expr args multiline ->
            FunctionApplication
                (JustF $ fromRawAST config expr)
                (fmap (\(C comments a) -> fromRawAST config a) args)
                (FunctionApplicationDisplay False)

        AST.Binops first rest multiline ->
            BinaryOperatorList
                (fromRawAST config first)
                (fmap (\(AST.BinopsClause c1 op c2 expr) -> BinaryOperation (mkReference op) (fromRawAST config expr)) rest)

        AST.Unary op expr ->
            UnaryOperator
                op
                (fromRawAST config expr)

        AST.Parens (C comments expr) ->
            fromRawAST' config $ extract $ I.unFix expr

        AST.ExplicitList terms comments multiline ->
            ListLiteral
                (fmap (\(C comments a) -> fromRawAST config a) $ AST.toCommentedList terms)

        AST.Tuple terms multiline ->
            TupleLiteral
                (fmap (\(C comments a) -> fromRawAST config a) terms)

        AST.TupleFunction n | n <= 1 ->
            error ("INVALID TUPLE CONSTRUCTOR: " ++ show n)

        AST.TupleFunction n ->
            VariableReferenceExpression
                (mkReference $ OpRef $ SymbolIdentifier $ replicate (n-1) ',')

        AST.Record base fields comments multiline ->
            RecordLiteral
                (fmap (\(C comments a) -> a) base)
                (Map.fromList $ fmap (\(C cp (Pair (C ck key) (C cv value) ml)) -> (key, fromRawAST config value)) $ AST.toCommentedList fields)
                $ RecordDisplay
                    (fmap (extract . _key . extract) $ AST.toCommentedList fields)

        AST.Access base field ->
            FunctionApplication
                (NothingF $ RecordAccessFunction field)
                [ fromRawAST config base ]
                (FunctionApplicationDisplay True)

        AST.Lambda parameters comments body multiline ->
            AnonymousFunction
                (fmap (\(C c a) -> fromRawAST config a) parameters)
                (fromRawAST config body)

        AST.If (AST.IfClause cond' thenBody') rest' (C c3 elseBody) ->
            ifThenElse cond' thenBody' rest'
            where
                ifThenElse (C c1 cond) (C c2 thenBody) rest =
                    IfExpression
                        (fromRawAST config cond)
                        (fromRawAST config thenBody)
                        $ case rest of
                            [] -> JustF $ fromRawAST config elseBody
                            C c4 (AST.IfClause nextCond nextBody) : nextRest ->
                                NothingF $ ifThenElse nextCond nextBody nextRest

        AST.Let decls comments body ->
            LetExpression
                (mkLetDeclarations config decls)
                (fromRawAST config body)

        AST.Case (C comments subject, multiline) branches ->
            CaseExpression
                (fromRawAST config subject)
                (fmap (fromRawAST config) branches)

        AST.Range _ _ _ ->
            error "Range syntax is not supported in Elm 0.19"

        AST.AccessFunction field ->
            RecordAccessFunction field

        AST.GLShader shader ->
            GLShader shader

instance FromPublicAST 'ExpressionNK where
    toRawAST' = \case
        UnitLiteral ->
            AST.Unit []

instance ToJSON Expression where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs Expression where
    toPairs = \case
        UnitLiteral ->
            mconcat
                [ type_ "UnitLiteral"
                ]

        LiteralExpression lit ->
            toPairs lit

        VariableReferenceExpression ref ->
            toPairs ref

        FunctionApplication function arguments display ->
            mconcat $ Maybe.catMaybes
                [ Just $ type_ "FunctionApplication"
                , Just $ "function" .= function
                , Just $ "arguments" .= arguments
                , fmap (pair "display") $ toMaybeEncoding display
                ]

        BinaryOperatorList first operations ->
            mconcat
                [ type_ "BinaryOperatorList"
                , "first" .= first
                , "operations" .= operations
                ]

        UnaryOperator operator term ->
            mconcat
                [ type_ "UnaryOperator"
                , "operator" .= operator
                , "term" .= term
                ]

        ListLiteral terms ->
            mconcat
                [ type_ "ListLiteral"
                , "terms" .= terms
                ]

        TupleLiteral terms ->
            mconcat
                [ type_ "TupleLiteral"
                , "terms" .= terms
                ]

        RecordLiteral Nothing fields display ->
            mconcat
                [ type_ "RecordLiteral"
                , "fields" .= fields
                , "display" .= display
                ]

        RecordLiteral (Just base) fields display ->
            mconcat
                [ type_ "RecordUpdate"
                , "base" .= base
                , "fields" .= fields
                , "display" .= display
                ]

        RecordAccessFunction field ->
            mconcat
                [ type_ "RecordAccessFunction"
                , "field" .= field
                ]

        AnonymousFunction parameters body ->
            mconcat
                [ type_ "AnonymousFunction"
                , "parameters" .= parameters
                , "body" .= body
                ]

        IfExpression if_ then_ else_ ->
            mconcat
                [ type_ "IfExpression"
                , "if" .= if_
                , "then" .= then_
                , "else" .= else_
                ]

        LetExpression declarations body ->
            mconcat
                [ type_ "LetExpression"
                , "declarations" .= declarations
                , "body" .= body
                ]

        CaseExpression subject branches ->
            mconcat
                [ type_ "CaseExpression"
                , "subject" .= subject
                , "branches" .= branches
                ]

        GLShader shaderSource ->
            mconcat
                [ type_ "GLShader"
                , "shaderSource" .= shaderSource
                ]

instance FromJSON Expression where
    parseJSON = withObject "Expression" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "UnitLiteral" ->
                return UnitLiteral

            _ ->
                fail ("unexpected Expression tag: " <> tag)


--
-- Transformation from raw AST to PublicAST
--


class ToPublicAST (nk :: NodeKind) where
    type PublicAST nk
    fromRawAST' :: Config -> ASTNS1 Located [UppercaseIdentifier] nk -> PublicAST nk

fromRawAST :: ToPublicAST nk => Config -> ASTNS Located [UppercaseIdentifier] nk -> LocatedIfRequested (PublicAST nk)
fromRawAST config =
    fmap (fromRawAST' config) . fromLocated config . I.unFix


class ToPublicAST nk => FromPublicAST (nk :: NodeKind) where
    toRawAST' :: PublicAST nk -> ASTNS1 Identity [UppercaseIdentifier] nk

toRawAST :: FromPublicAST nk => LocatedIfRequested (PublicAST nk) -> ASTNS Identity [UppercaseIdentifier] nk
toRawAST =
    I.Fix . Identity . toRawAST' . extract


instance ToPublicAST 'PatternNK where
    type PublicAST 'PatternNK = Pattern

    fromRawAST' config = \case
        AST.Anything ->
            AnythingPattern

        AST.UnitPattern comments ->
            UnitPattern

        AST.LiteralPattern lit ->
            LiteralPattern lit

        AST.VarPattern name ->
            VariablePattern $ VariableDefinition name

        AST.OpPattern _ ->
            error "PublicAST: OpPattern is not supported in Elm 0.19"

        AST.DataPattern (namespace, tag) args ->
            DataPattern
                (mkReference (TagRef namespace tag))
                (fmap (fromRawAST config . (\(C comments a) -> a)) args)

        AST.PatternParens (C (pre, post) pat) ->
            extract $ fromRawAST config pat

        AST.TuplePattern terms ->
            TuplePattern
                (fmap (fromRawAST config . (\(C comments a) -> a)) terms)

        AST.EmptyListPattern comments ->
            mkListPattern [] Nothing

        AST.ListPattern terms ->
            mkListPattern
                (fmap (fromRawAST config . (\(C comments a) -> a)) terms)
                Nothing

        AST.ConsPattern (C firstEol first) rest ->
            let
                first' = fromRawAST config first
                rest' = fmap (fromRawAST config . (\(C comments a) -> a)) (AST.toCommentedList rest)
            in
            case reverse rest' of
                [] -> mkListPattern [] (Just first')
                last : mid -> mkListPattern (first' : reverse mid) (Just last)

        AST.EmptyRecordPattern comment ->
            RecordPattern []

        AST.RecordPattern fields ->
            RecordPattern
                (fmap (VariableDefinition . (\(C comments a) -> a)) fields)

        AST.Alias (C comments1 pat) (C comments2 name) ->
            PatternAlias
                (VariableDefinition name)
                (fromRawAST config pat)


instance ToPublicAST 'TypeNK where
    type PublicAST 'TypeNK = Type_

    fromRawAST' config = \case
        AST.UnitType comments ->
            UnitType

        AST.TypeConstruction (AST.NamedConstructor (namespace, name)) args forceMultine ->
            TypeReference
                name
                (ModuleName namespace)
                (fmap (\(C comments a) -> fromRawAST config a) args)

        AST.TypeVariable name ->
            TypeVariable name

        AST.TypeParens (C comments t) ->
            fromRawAST' config (extract $ I.unFix t)

        AST.TupleType terms multiline ->
            TupleType
                (fmap (\(C comments a) -> fromRawAST config a) terms)

        AST.RecordType base fields comments multiline ->
            RecordType
                (fmap (\(C comments a) -> a) base)
                (Map.fromList $ fmap (\(C cp (Pair (C ck key) (C cv value) ml)) -> (key, fromRawAST config value)) $ AST.toCommentedList fields)
                $ RecordDisplay
                    (fmap (extract . _key . extract) $ AST.toCommentedList fields)

        AST.FunctionType first rest multiline ->
            case firstRestToRestLast first (AST.toCommentedList rest) of
                (args, C comments last) ->
                    FunctionType
                        (fromRawAST config last)
                        (fmap (\(C comments a) -> fromRawAST config a) args)
        where
            firstRestToRestLast :: AST.C0Eol x -> List (AST.C2Eol a b x) -> (List (AST.C2Eol a b x), AST.C0Eol x)
            firstRestToRestLast first rest =
                done $ foldl (flip step) (ReversedList.empty, first) rest
                where
                    step :: AST.C2Eol a b x -> (Reversed (AST.C2Eol a b x), AST.C0Eol x) -> (Reversed (AST.C2Eol a b x), AST.C0Eol x)
                    step (C (a, b, dn) next) (acc, C dn' last) =
                        (ReversedList.push (C (a, b, dn') last) acc, C dn next)

                    done :: (Reversed (AST.C2Eol a b x), AST.C0Eol x) -> (List (AST.C2Eol a b x), AST.C0Eol x)
                    done (acc, last) =
                        (ReversedList.toList acc, last)

instance FromPublicAST 'TypeNK where
    toRawAST' = \case
        UnitType ->
            AST.UnitType []


--
-- JSON serialization
--


data Config =
    Config
        { showSourceLocation :: Bool
        }


class ToPairs a where
    toPairs :: a -> Series


class ToMaybeJSON a where
    toMaybeEncoding :: a -> Maybe Encoding


type_ :: String -> Series
type_ t =
    "tag" .= t


instance ToPairs a => ToJSON (Located a) where
    toJSON = undefined
    toEncoding (A region a) =
        pairs (toPairs a <> "sourceLocation" .= region)


instance ToJSON Region where
    toJSON = undefined
    toEncoding region =
        pairs $ mconcat
            [ "start" .= Region.start region
            , "end" .= Region.end region
            ]


instance ToJSON Region.Position where
    toJSON = undefined
    toEncoding pos =
        pairs $ mconcat
            [ "line" .= Region.line pos
            , "col" .= Region.column pos
            ]


instance ToJSON UppercaseIdentifier where
    toJSON = undefined
    toEncoding (UppercaseIdentifier name) = toEncoding name

instance FromJSON UppercaseIdentifier where
    parseJSON = withText "UppercaseIdentifier" $ \case
        -- XXX: shouldn't crash on empty string
        text | Char.isUpper $ Text.head text ->
            return $ UppercaseIdentifier $ Text.unpack text

        _ ->
            fail "expected a string starting with an uppercase letter"


instance ToJSON LowercaseIdentifier where
    toJSON = undefined
    toEncoding (LowercaseIdentifier name) = toEncoding name
instance ToJSONKey LowercaseIdentifier where
    toJSONKey =
        ToJSONKeyText
            (\(LowercaseIdentifier name) -> Text.pack name)
            (\(LowercaseIdentifier name) -> AesonInternal.string name)

instance FromJSON LowercaseIdentifier where
    parseJSON = withText "LowercaseIdentifier" $ \case
        -- XXX: shouldn't crash on empty string
        text | Char.isLower $ Text.head text ->
            return $ LowercaseIdentifier $ Text.unpack text

        _ ->
            fail "expected a string starting with a lowercase letter"


instance ToJSON SymbolIdentifier where
    toJSON = undefined
    toEncoding (SymbolIdentifier sym) = toEncoding sym


instance ToJSON (Ref ()) where
    toJSON = undefined
    toEncoding (VarRef () var) = toEncoding var
    toEncoding (TagRef () tag) = toEncoding tag
    toEncoding (OpRef sym) = toEncoding sym


instance ToJSON AST.UnaryOperator where
    toJSON = undefined
    toEncoding Negative = toEncoding ("-" :: Text)


instance ToJSON (AST.Listing AST.DetailedListing) where
    toJSON = undefined
    toEncoding = \case
        AST.ExplicitListing a comments -> toEncoding a
        AST.OpenListing (C comments ()) -> toEncoding ("Everything" :: Text)
        AST.ClosedListing -> toEncoding Null


instance ToJSON AST.DetailedListing where
    toJSON = undefined
    toEncoding = \case
        AST.DetailedListing values operators types ->
            pairs $ mconcat
                [ "values" .= Map.fromList (fmap (\(LowercaseIdentifier k) -> (k, True)) (Map.keys values))
                , "types" .= Map.fromList (fmap (\(UppercaseIdentifier k, (C _ (C _ listing))) -> (k, listing)) (Map.toList types))
                ]


instance ToJSON (AST.Listing (AST.CommentedMap UppercaseIdentifier ())) where
    toJSON = undefined
    toEncoding = \case
        AST.ExplicitListing tags _ ->
            toEncoding $ Map.fromList $ fmap (\(UppercaseIdentifier k, C _ ()) -> (k, True)) $ Map.toList tags
        AST.OpenListing (C _ ()) -> toEncoding ("AllTags" :: Text)
        AST.ClosedListing -> toEncoding ("NoTags" :: Text)


instance ToJSON IntRepresentation where
    toEncoding = genericToEncoding defaultOptions


instance ToJSON FloatRepresentation where
    toEncoding = genericToEncoding defaultOptions


instance ToJSON StringRepresentation where
    toEncoding = genericToEncoding defaultOptions


instance ToJSON ModuleName where
    toJSON = undefined
    toEncoding (ModuleName []) = toEncoding Null
    toEncoding namespace = toEncoding $ show namespace


instance ToJSON AST.LiteralValue where
    toJSON = undefined
    toEncoding = pairs . toPairs

instance ToPairs AST.LiteralValue where
    toPairs = \case
        IntNum value repr ->
            mconcat
                [ type_ "IntLiteral"
                , "value" .= value
                , pair "display" $ pairs
                    ("representation" .= repr)
                ]

        FloatNum value repr ->
            mconcat
                [ type_ "FloatLiteral"
                , "value" .= value
                , pair "display" $ pairs
                    ("representation" .= repr)
                ]

        Boolean value ->
            toPairs $
                ExternalReference
                    (ModuleName [UppercaseIdentifier "Basics"])
                    (TagRef () $ UppercaseIdentifier $ show value)

        Chr chr ->
            mconcat
                [ type_ "CharLiteral"
                , "value" .= chr
                ]

        Str str repr ->
            mconcat
                [ type_ "StringLiteral"
                , "value" .= str
                , pair "display" $ pairs
                    ("representation" .= repr)
                ]


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Reporting.Annotation.Located a
noRegion =
    Reporting.Annotation.at nowhere nowhere
