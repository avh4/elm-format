{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ElmFormat.AST.PublicAST where

import ElmFormat.AST.Shared
import AST.V0_16 (NodeKind(..), Pair(..))
import qualified AST.V0_16 as AST
import qualified AST.Module as AST
import qualified AST.Listing as AST
import Reporting.Annotation (Located(A))
import Reporting.Region (Region)
import qualified Reporting.Region as Region
import Text.JSON hiding (showJSON)
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
import Data.Aeson hiding (ToJSON)


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


data ModuleName =
    ModuleName [UppercaseIdentifier]
    deriving (Eq, Ord)

instance Show ModuleName where
    show (ModuleName ns) = List.intercalate "." $ fmap (\(UppercaseIdentifier v) -> v) ns


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
    showJSON = \case
        Comment text display ->
            makeObj
                [ type_ "Comment"
                , ( "text", JSString $ toJSString text )
                , ( "display", showJSON display )
                ]

data CommentDisplay =
    CommentDisplay
        { commentType :: CommentType
        }

instance ToJSON CommentDisplay where
    showJSON = \case
        CommentDisplay commentType ->
            makeObj
                [ ( "commentType", showJSON commentType )
                ]

data CommentType
    = BlockComment
    | LineComment

instance ToJSON CommentType where
    showJSON = \case
        BlockComment -> JSString $ toJSString "BlockComment"
        LineComment -> JSString $ toJSString "LineComment"


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
            (Just $ C ([], []) $ AST.OpenListing (C ([], []) ()))
        )
        (noRegion Nothing)
        (C [] mempty)
        (f $ AST.TopLevel
            [ AST.Entry $ f $ AST.Definition
                (f $ AST.VarPattern $ LowercaseIdentifier "f")
                []
                []
                (f $ AST.Unit [])
            ]
        )
    where
        f = I.Fix . Identity

instance ToJSON Module where
    showJSON = \case
        Module moduleName imports body ->
            makeObj
                [ ( "moduleName", showJSON moduleName )
                , ( "imports", showJSON imports )
                , ( "body", showJSON body )
                ]

instance FromJSON Module where
    parseJSON = withObject "Module" $ \obj -> do
        moduleName <- obj .: "moduleName"
        -- TODO: parse imports
        -- TODO: parse body
        return $ Module
            (ModuleName [ UppercaseIdentifier moduleName ])
            mempty
            []


data Import
    = Import
        { as :: ModuleName
        , exposing :: AST.Listing AST.DetailedListing
        }

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
    showJSON = \case
        Import as exposing ->
            makeObj
                [ ( "as", showJSON as )
                , ( "exposing", showJSON exposing )
                ]

data TypedParameter
    = TypedParameter
        { pattern :: LocatedIfRequested Pattern
        , type_tp :: Maybe (LocatedIfRequested Type_)
        }

instance ToJSON TypedParameter where
    showJSON = \case
        TypedParameter pattern typ ->
            makeObj
                [ ( "pattern", showJSON pattern )
                , ( "type", showJSON typ )
                ]


data CustomTypeVariant
    = CustomTypeVariant
        { name :: UppercaseIdentifier
        , parameterTypes :: List (LocatedIfRequested Type_)
        }

instance ToJSON CustomTypeVariant where
    showJSON = \case
        CustomTypeVariant name parameterTypes ->
            makeObj
                [ ( "name", showJSON name )
                , ( "parameterTypes", showJSON parameterTypes )
                ]

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
    showJSON = \case
        Definition name parameters returnType expression ->
            makeObj
                [ type_ "Definition"
                , ( "name", showJSON name )
                , ( "parameters", showJSON parameters )
                , ( "returnType", showJSON returnType )
                , ( "expression", showJSON expression )
                ]

        TODO_Definition info ->
            makeObj
                [ type_ "TODO: Definition"
                , ( "$", JSArray $ fmap (JSString . toJSString) info )
                ]


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

instance ToJSON TopLevelStructure where
    showJSON = \case
        DefinitionStructure def ->
            showJSON def

        TypeAlias name parameters t ->
            makeObj
                [ type_ "TypeAlias"
                , ( "name", showJSON name )
                , ( "type", showJSON t )
                ]

        CustomType name parameters variants ->
            makeObj
                [ type_ "CustomType"
                , ( "name", showJSON name )
                , ( "parameters", showJSON parameters )
                , ( "variants", showJSON variants )
                ]

        Comment_tls comment ->
            showJSON comment

        TODO_TopLevelStructure s ->
            JSString $ toJSString s


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


data VariableDefinition
    = VariableDefinition
        { name :: LowercaseIdentifier
        }


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


newtype RecordDisplay
    = RecordDisplay
        { fieldOrder :: List LowercaseIdentifier
        }


data BinaryOperation
    = BinaryOperation
        { operator :: Reference
        , term :: LocatedIfRequested Expression
        }

instance ToJSON BinaryOperation where
    showJSON = \case
        BinaryOperation operator term ->
            makeObj
                [ ( "operator", showJSON operator )
                , ( "term", showJSON term )
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
    showJSON = \case
        LetDefinition def ->
            showJSON def

        Comment_ld comment ->
            showJSON comment


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

instance ToJSON CaseBranch where
    showJSON = \case
        CaseBranch pattern body ->
            makeObj
                [ ( "pattern", showJSON pattern )
                , ( "body", showJSON body )
                ]


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

instance ToJSON FunctionApplicationDisplay where
    showJSON = \case
        FunctionApplicationDisplay showAsRecordAccess ->
            makeObj $ Maybe.catMaybes
                [ if showAsRecordAccess
                    then Just ( "showAsRecordAccess", JSBool True )
                    else Nothing
                ]


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
    showJSON = \case
        JustF fa -> showJSON fa
        NothingF a -> showJSON a


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


instance ToJSON Expression where
    showJSON = \case
        UnitLiteral ->
            makeObj
                [ type_ "UnitLiteral"
                ]

        LiteralExpression lit ->
            showJSON lit

        VariableReferenceExpression ref ->
            showJSON ref

        FunctionApplication function arguments display ->
            makeObj $ mapMaybe removeEmpty
                [ type_ "FunctionApplication"
                , ( "function", showJSON function )
                , ( "arguments", showJSON arguments)
                , ( "display", showJSON display )
                ]

        BinaryOperatorList first operations ->
            makeObj
                [ type_ "BinaryOperatorList"
                , ( "first", showJSON first )
                , ( "operations", showJSON operations )
                ]

        UnaryOperator operator term ->
            makeObj
                [ type_ "UnaryOperator"
                , ( "operator", showJSON operator )
                , ( "term", showJSON term )
                ]

        ListLiteral terms ->
            makeObj
                [ type_ "ListLiteral"
                , ( "terms", showJSON terms)
                ]

        TupleLiteral terms ->
            makeObj
                [ type_ "TupleLiteral"
                , ( "terms", showJSON terms)
                ]

        RecordLiteral Nothing fields display ->
            makeObj
                [ type_ "RecordLiteral"
                , ( "fields", showJSON fields )
                , ( "display", showJSON display )
                ]

        RecordLiteral (Just base) fields display ->
            makeObj
                [ type_ "RecordUpdate"
                , ( "base", showJSON base )
                , ( "fields", showJSON fields )
                , ( "display", showJSON display )
                ]

        RecordAccessFunction field ->
            makeObj
                [ type_ "RecordAccessFunction"
                , ( "field", showJSON field )
                ]

        AnonymousFunction parameters body ->
            makeObj
                [ type_ "AnonymousFunction"
                , ( "parameters", showJSON parameters )
                , ( "body", showJSON body )
                ]

        IfExpression if_ then_ else_ ->
            makeObj
                [ type_ "IfExpression"
                , ( "if", showJSON if_ )
                , ( "then", showJSON then_ )
                , ( "else", showJSON else_ )
                ]

        LetExpression declarations body ->
            makeObj
                [ type_ "LetExpression"
                , ( "declarations", showJSON declarations )
                , ( "body", showJSON body )
                ]

        CaseExpression subject branches ->
            makeObj
                [ type_ "CaseExpression"
                , ( "subject", showJSON subject )
                , ( "branches", showJSON branches )
                ]

        GLShader shaderSource ->
            makeObj
                [ type_ "GLShader"
                , ( "shaderSource", JSString $ toJSString shaderSource )
                ]


--
-- Transformation from raw AST to PublicAST
--


class ToPublicAST (nk :: NodeKind) where
    type PublicAST nk
    fromRawAST' :: Config -> ASTNS1 Located [UppercaseIdentifier] nk -> PublicAST nk


fromRawAST :: ToPublicAST nk => Config -> ASTNS Located [UppercaseIdentifier] nk -> LocatedIfRequested (PublicAST nk)
fromRawAST config =
    fmap (fromRawAST' config) . fromLocated config . I.unFix


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


--
-- JSON serialization
--


data Config =
    Config
        { showSourceLocation :: Bool
        }


class ToJSON a where
  showJSON :: a -> JSValue


addField :: ( String, JSValue ) -> JSValue -> JSValue
addField field = \case
    JSObject obj ->
        JSObject
            $ toJSObject
            $ (++ [ field ])
            $ fromJSObject obj

    otherJson ->
        makeObj
            [ ( "value", otherJson )
            , field
            ]


removeEmpty :: ( String, JSValue ) -> Maybe ( String, JSValue )
removeEmpty (key, val) =
    case val of
        JSObject obj ->
            case fromJSObject obj of
                [] -> Nothing
                _ -> Just (key, val)
        _ -> Just (key, val)


type_ :: String -> (String, JSValue)
type_ t =
    ("tag", JSString $ toJSString t)


instance ToJSON a => ToJSON [a] where
    showJSON = JSArray . fmap (showJSON)


instance ToJSON a => ToJSON (Maybe a) where
    showJSON Nothing = JSNull
    showJSON (Just a) = showJSON a


instance (Show k, ToJSON v) => ToJSON (Map k v) where
    showJSON =
        makeObj
            . fmap (\(k, a) -> (show k, showJSON a))
            . Map.toList


instance ToJSON a => ToJSON (Located a) where
    showJSON (A region a) =
        addField ( "sourceLocation", showJSON region ) (showJSON a)


instance ToJSON Region where
    showJSON region =
        makeObj
            [ ( "start", showJSON $ Region.start region )
            , ( "end", showJSON $ Region.end region )
            ]


instance ToJSON a => ToJSON (LocatedIfRequested a) where
    showJSON (LocatedIfRequested showSourceLocation (A region a)) =
        if showSourceLocation
            then showJSON (A region a)
            else showJSON a


instance ToJSON Region.Position where
    showJSON pos =
        makeObj
            [ ( "line", JSRational False $ toRational $ Region.line pos )
            , ( "col", JSRational False $ toRational $ Region.column pos )
            ]


instance ToJSON UppercaseIdentifier where
    showJSON (UppercaseIdentifier name) = JSString $ toJSString name


instance ToJSON LowercaseIdentifier where
    showJSON (LowercaseIdentifier name) = JSString $ toJSString name


instance ToJSON SymbolIdentifier where
    showJSON (SymbolIdentifier sym) = JSString $ toJSString sym


instance ToJSON (Ref ()) where
    showJSON (VarRef () var) = showJSON var
    showJSON (TagRef () tag) = showJSON tag
    showJSON (OpRef sym) = showJSON sym


instance ToJSON AST.UnaryOperator where
    showJSON Negative = JSString $ toJSString "-"


instance ToJSON (AST.Listing AST.DetailedListing) where
    showJSON = \case
        AST.ExplicitListing a comments -> showJSON a
        AST.OpenListing (C comments ()) -> JSString $ toJSString "Everything"
        AST.ClosedListing -> JSNull


instance ToJSON AST.DetailedListing where
    showJSON = \case
        AST.DetailedListing values operators types ->
            makeObj
                [ ( "values", makeObj $ fmap (\(LowercaseIdentifier k) -> (k, JSBool True)) $ Map.keys values )
                , ( "types", makeObj $ fmap (\(UppercaseIdentifier k, (C _ (C _ listing))) -> (k, showTagListingJSON listing)) $ Map.toList types )
                ]


showTagListingJSON :: AST.Listing (AST.CommentedMap UppercaseIdentifier ()) -> JSValue
showTagListingJSON = \case
    AST.ExplicitListing tags _ ->
        makeObj $ fmap (\(UppercaseIdentifier k, C _ ()) -> (k, JSBool True)) $ Map.toList tags
    AST.OpenListing (C _ ()) -> JSString $ toJSString "AllTags"
    AST.ClosedListing -> JSString $ toJSString "NoTags"


instance ToJSON IntRepresentation where
    showJSON DecimalInt = JSString $ toJSString "DecimalInt"
    showJSON HexadecimalInt = JSString $ toJSString "HexadecimalInt"


instance ToJSON FloatRepresentation where
    showJSON DecimalFloat = JSString $ toJSString "DecimalFloat"
    showJSON ExponentFloat = JSString $ toJSString "ExponentFloat"


instance ToJSON StringRepresentation where
    showJSON SingleQuotedString = JSString $ toJSString "SingleQuotedString"
    showJSON TripleQuotedString = JSString $ toJSString "TripleQuotedString"


instance ToJSON ModuleName where
    showJSON (ModuleName []) = JSNull
    showJSON namespace = JSString $ toJSString $ show namespace


instance ToJSON AST.LiteralValue where
    showJSON = \case
        IntNum value repr ->
            makeObj
                [ type_ "IntLiteral"
                , ("value", JSRational False $ toRational value)
                , ("display"
                , makeObj
                    [ ("representation", showJSON repr)
                    ]
                )
                ]

        FloatNum value repr ->
            makeObj
                [ type_ "FloatLiteral"
                , ("value", JSRational False $ toRational value)
                , ("display"
                , makeObj
                    [ ("representation", showJSON repr)
                    ]
                )
                ]

        Boolean value ->
            showJSON $
                ExternalReference
                    (ModuleName [UppercaseIdentifier "Basics"])
                    (TagRef () $ UppercaseIdentifier $ show value)

        Chr chr ->
            makeObj
                [ type_ "CharLiteral"
                , ("value", JSString $ toJSString [chr])
                ]

        Str str repr ->
            makeObj
                [ type_ "StringLiteral"
                , ("value", JSString $ toJSString str)
                , ( "display"
                , makeObj
                    [ ( "representation", showJSON repr )
                    ]
                )
                ]


instance ToJSON Pattern where
    showJSON = \case
        AnythingPattern ->
            makeObj
                [ type_ "AnythingPattern"
                ]

        UnitPattern ->
            makeObj
                [ type_ "UnitPattern"
                ]

        LiteralPattern lit ->
            showJSON lit

        VariablePattern def ->
            showJSON def

        DataPattern constructor arguments ->
            makeObj
                [ type_ "DataPattern"
                , ( "constructor", showJSON constructor )
                , ( "arguments", showJSON arguments )
                ]

        TuplePattern terms ->
            makeObj
                [ type_ "TuplePattern"
                , ( "terms", showJSON terms )
                ]

        ListPattern prefix rest ->
            makeObj
                [ type_ "ListPattern"
                , ( "prefix", showJSON prefix )
                , ( "rest", showJSON rest )
                ]

        RecordPattern fields ->
            makeObj
                [ type_ "RecordPattern"
                , ( "fields", showJSON fields )
                ]

        PatternAlias alias pat ->
            makeObj
                [ type_ "PatternAlias"
                , ( "alias", showJSON alias )
                , ( "pattern", showJSON pat )
                ]


instance ToJSON Type_ where
    showJSON = \case
        UnitType ->
            makeObj
                [ type_ "UnitType"
                ]

        TypeReference name module_ arguments ->
            makeObj
                [ type_ "TypeReference"
                , ( "name", showJSON name )
                , ( "module", showJSON module_ )
                , ( "arguments", showJSON arguments )
                ]

        TypeVariable name ->
            makeObj
                [ type_ "TypeVariable"
                , ( "name", showJSON name )
                ]

        TupleType terms ->
            makeObj
                [ type_ "TupleType"
                , ( "terms", showJSON terms )
                ]

        RecordType Nothing fields display ->
            makeObj
                [ type_ "RecordType"
                , ( "fields", showJSON fields )
                , ( "display", showJSON display )
                ]

        RecordType (Just base) fields display ->
            makeObj
                [ type_ "RecordTypeExtension"
                , ( "base", showJSON base )
                , ( "fields", showJSON fields )
                , ( "display", showJSON display )
                ]

        FunctionType returnType argumentTypes ->
            makeObj
                [ type_ "FunctionType"
                , ( "returnType", showJSON returnType )
                , ( "argumentTypes", showJSON argumentTypes )
                ]


instance ToJSON RecordDisplay where
    showJSON (RecordDisplay fieldOrder) =
        makeObj
            [ ( "fieldOrder", showJSON fieldOrder )
            ]


instance ToJSON Reference where
    showJSON = \case
        ExternalReference module_ identifier ->
            makeObj
                [ type_ "ExternalReference"
                , ("module", showJSON module_)
                , ("identifier", showJSON identifier)
                ]

        VariableReference name ->
            makeObj
                [ type_ "VariableReference"
                , ( "name", showJSON name )
                ]


instance ToJSON VariableDefinition where
    showJSON (VariableDefinition name) =
        makeObj
            [ type_ "VariableDefinition"
            , ( "name" , showJSON name )
            ]


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Reporting.Annotation.Located a
noRegion =
    Reporting.Annotation.at nowhere nowhere
