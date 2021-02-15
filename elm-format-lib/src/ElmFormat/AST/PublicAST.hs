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
    showJSON c = \case
        Comment text display ->
            makeObj
                [ type_ "Comment"
                , ( "text", JSString $ toJSString text )
                , ( "display", showJSON c display )
                ]

data CommentDisplay =
    CommentDisplay
        { commentType :: CommentType
        }

instance ToJSON CommentDisplay where
    showJSON c = \case
        CommentDisplay commentType ->
            makeObj
                [ ( "commentType", showJSON c commentType )
                ]

data CommentType
    = BlockComment
    | LineComment

instance ToJSON CommentType where
    showJSON _ = \case
        BlockComment -> JSString $ toJSString "BlockComment"
        LineComment -> JSString $ toJSString "LineComment"


data Module
    = Module
        { moduleName :: ModuleName
        , imports :: Map ModuleName Import
        , body :: List (MaybeF Located TopLevelStructure)
        }

fromModule :: AST.Module [UppercaseIdentifier] (ASTNS Located [UppercaseIdentifier] 'TopLevelNK) -> Module
fromModule = \case
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
            (fromTopLevelStructures $ normalize body)

instance ToJSON Module where
    showJSON c = \case
        Module moduleName imports body ->
            makeObj
                [ ( "moduleName", showJSON c moduleName )
                , ( "imports", showJSON c imports )
                , ( "body", showJSON c body )
                ]


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
    showJSON c = \case
        Import as exposing ->
            makeObj
                [ ( "as", showJSON c as )
                , ( "exposing", showJSON c exposing )
                ]

data TypedParameter
    = TypedParameter
        { pattern :: Located Pattern
        , type_tp :: Maybe (Located Type_)
        }

instance ToJSON TypedParameter where
    showJSON c = \case
        TypedParameter pattern typ ->
            makeObj
                [ ( "pattern", showJSON c pattern )
                , ( "type", showJSON c typ )
                ]


data CustomTypeVariant
    = CustomTypeVariant
        { name :: UppercaseIdentifier
        , parameterTypes :: List (Located Type_)
        }

instance ToJSON CustomTypeVariant where
    showJSON c = \case
        CustomTypeVariant name parameterTypes ->
            makeObj
                [ ( "name", showJSON c name )
                , ( "parameterTypes", showJSON c parameterTypes )
                ]

mkCustomTypeVariant :: AST.NameWithArgs UppercaseIdentifier (ASTNS Located [UppercaseIdentifier] 'TypeNK) -> CustomTypeVariant
mkCustomTypeVariant (AST.NameWithArgs name args) =
    CustomTypeVariant
        name
        (fmap (\(C c a) -> fromRawAST a) args)

data Definition
    = Definition
        { name_d :: LowercaseIdentifier
        , parameters_d :: List TypedParameter
        , returnType :: Maybe (Located Type_)
        , expression :: Located Expression
        }
    | TODO_Definition (List String)

mkDefinition ::
    ASTNS1 Located [UppercaseIdentifier] 'PatternNK
    -> List (AST.C1 'AST.BeforeTerm (ASTNS Located [UppercaseIdentifier] 'PatternNK))
    -> Maybe (AST.Comments, AST.Comments, ASTNS Located [UppercaseIdentifier] 'TypeNK)
    -> ASTNS Located [UppercaseIdentifier] 'ExpressionNK
    -> Definition
mkDefinition pat args annotation expr =
    case pat of
        AST.VarPattern name ->
            let
                (typedParams, returnType) =
                    maybe
                        ( fmap (\a -> ( a, Nothing ) ) args, Nothing )
                        ( (\(a,b) -> (fmap (fmap Just) a, Just b)) . PatternMatching.matchType args . (\(c1, c2, t) -> t))
                        annotation
            in
            Definition
                name
                (fmap (\(C c pat, typ) -> TypedParameter (fromRawAST pat) (fmap fromRawAST typ)) typedParams)
                (fmap fromRawAST returnType)
                (fromRawAST expr)

        _ ->
            TODO_Definition
                [ show pat
                , show args
                , show annotation
                , show expr
                ]

instance ToJSON Definition where
    showJSON c = \case
        Definition name parameters returnType expression ->
            makeObj
                [ type_ "Definition"
                , ( "name", showJSON c name )
                , ( "parameters", showJSON c parameters )
                , ( "returnType", showJSON c returnType )
                , ( "expression", showJSON c expression )
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
        , type_ta :: Located Type_
        }
    | CustomType
        { name_ct :: UppercaseIdentifier
        , parameters_ct :: List LowercaseIdentifier
        , variants :: List CustomTypeVariant
        }
    | Comment_tls Comment
    | TODO_TopLevelStructure String


fromTopLevelStructures :: ASTNS Located [UppercaseIdentifier] 'TopLevelNK -> List (MaybeF Located TopLevelStructure)
fromTopLevelStructures (I.Fix (A _ (AST.TopLevel decls))) =
    let
        collectAnnotation :: AST.TopLevelStructure (ASTNS Located ns 'DeclarationNK) -> Maybe (LowercaseIdentifier, (AST.Comments, AST.Comments, ASTNS Located ns 'TypeNK))
        collectAnnotation decl =
            case fmap (extract . I.unFix) decl of
                AST.Entry (AST.TypeAnnotation (C preColon (VarRef () name)) (C postColon typ)) -> Just (name, (preColon, postColon, typ))
                _ -> Nothing

        annotations :: Map.Map LowercaseIdentifier (AST.Comments, AST.Comments, ASTNS Located [UppercaseIdentifier] 'TypeNK)
        annotations =
            Map.fromList $ mapMaybe collectAnnotation decls

        merge :: AST.TopLevelStructure
                     (ASTNS Located [UppercaseIdentifier] 'DeclarationNK) -> Maybe (MaybeF Located TopLevelStructure)
        merge decl =
            case fmap I.unFix decl of
                AST.Entry (A region (AST.Definition (I.Fix (A _ pat)) args preEquals expr)) ->
                    let
                        annotation =
                            case pat of
                                AST.VarPattern name ->
                                    Map.lookup name annotations
                                _ -> Nothing
                    in
                    Just $ JustF $ A region $ DefinitionStructure $
                        mkDefinition pat args annotation expr

                AST.Entry (A _ (AST.TypeAnnotation _ _)) ->
                    -- TODO: retain annotations that don't have a matching definition
                    Nothing

                AST.Entry (A region (AST.TypeAlias c1 (C (c2, c3) (AST.NameWithArgs name args)) (C c4 t))) ->
                    Just $ JustF $ A region $ TypeAlias name (fmap (\(C c a) -> a) args) (fromRawAST t)

                AST.Entry (A region (AST.Datatype (C (c1, c2) (AST.NameWithArgs name args)) variants)) ->
                    Just $ JustF $ A region $ CustomType
                        name
                        (fmap (\(C c a) -> a) args)
                        (fmap (\(C c a) -> mkCustomTypeVariant a) $ AST.toCommentedList variants)

                AST.Entry (A region d) ->
                    Just $ JustF $ A region $ TODO_TopLevelStructure ("TODO: " ++ show d)

                AST.BodyComment comment ->
                    Just $ NothingF $ Comment_tls (mkComment comment)

                _ ->
                    Just $ NothingF $
                        TODO_TopLevelStructure ("TODO: " ++ show decl)
    in
    mapMaybe merge decls

instance ToJSON TopLevelStructure where
    showJSON c = \case
        DefinitionStructure def ->
            showJSON c def

        TypeAlias name parameters t ->
            makeObj
                [ type_ "TypeAlias"
                , ( "name", showJSON c name )
                , ( "type", showJSON c t )
                ]

        CustomType name parameters variants ->
            makeObj
                [ type_ "CustomType"
                , ( "name", showJSON c name )
                , ( "parameters", showJSON c parameters )
                , ( "variants", showJSON c variants )
                ]

        Comment_tls comment ->
            showJSON c comment

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
        , arguments :: List (Located Pattern) -- Non-empty
        }
    | TuplePattern
        { terms :: List (Located Pattern) -- At least two items
        }
    | ListPattern -- Construct with mkListPattern
        { prefix :: List (Located Pattern)
        , rest :: Maybe (Located Pattern) -- Must not be a ListPattern
        }
    | RecordPattern
        { fields :: List VariableDefinition
        }
    | PatternAlias
        { alias :: VariableDefinition
        , pattern :: Located Pattern
        }


mkListPattern :: List (Located Pattern) -> Maybe (Located Pattern) -> Pattern
mkListPattern prefix (Just (A _ (ListPattern prefix2 rest2))) =
    ListPattern (prefix ++ prefix2) rest2
mkListPattern prefix rest = ListPattern prefix rest


data Type_
    = UnitType
    | TypeReference
        { name_tr :: UppercaseIdentifier
        , module_ :: ModuleName
        , arguments :: List (Located Type_)
        }
    | TypeVariable
        { name_tv :: LowercaseIdentifier
        }
    | TupleType
        { terms :: List (Located Type_) -- At least two items
        }
    | RecordType
        { base :: Maybe LowercaseIdentifier
        , fields :: Map LowercaseIdentifier (Located Type_) -- Cannot be empty if base is present
        , display :: RecordDisplay
        }
    | FunctionType
        { returnType :: Located Type_
        , argumentTypes :: List (Located Type_) -- Non-empty
        }


newtype RecordDisplay
    = RecordDisplay
        { fieldOrder :: List LowercaseIdentifier
        }


data BinaryOperation
    = BinaryOperation
        { operator :: Reference
        , term :: Located Expression
        }

instance ToJSON BinaryOperation where
    showJSON c = \case
        BinaryOperation operator term ->
            makeObj
                [ ( "operator", showJSON c operator )
                , ( "term", showJSON c term )
                ]


data LetDeclaration
    = Definition_ld
        { name :: LowercaseIdentifier
        , expression :: Located Expression
        }
    | Comment_ld Comment
    | TODO_LetDeclaration String

instance ToPublicAST 'LetDeclarationNK where
    type PublicAST 'LetDeclarationNK = LetDeclaration

    fromRawAST' = \case
        AST.LetDefinition (I.Fix (A region (AST.VarPattern var))) [] comments expr ->
            Definition_ld
                var
                (fromRawAST expr)

        AST.LetComment comment ->
            Comment_ld (mkComment comment)

        other ->
            TODO_LetDeclaration ("TODO: " ++ show other)

instance ToJSON LetDeclaration where
    showJSON c = \case
        Definition_ld name expression ->
            makeObj
                [ type_ "Definition"
                , ( "name", showJSON c name )
                , ( "expression", showJSON c expression )
                ]

        Comment_ld comment ->
            showJSON c comment

        TODO_LetDeclaration s ->
            JSString $ toJSString s


data CaseBranch
    = CaseBranch
        { pattern :: Located Pattern
        , body :: Located Expression
        }

instance ToPublicAST 'CaseBranchNK where
    type PublicAST 'CaseBranchNK = CaseBranch

    fromRawAST' = \case
        AST.CaseBranch c1 c2 c3 pat body ->
            CaseBranch
                (fromRawAST pat)
                (fromRawAST body)

instance ToJSON CaseBranch where
    showJSON c = \case
        CaseBranch pattern body ->
            makeObj
                [ ( "pattern", showJSON c pattern )
                , ( "body", showJSON c body )
                ]


data Expression
    = UnitLiteral
    | LiteralExpression LiteralValue
    | VariableReferenceExpression Reference
    | FunctionApplication
        { function :: MaybeF Located Expression
        , arguments :: List (Located Expression)
        , display_fa :: FunctionApplicationDisplay
        }
    | BinaryOperatorList
        { first :: Located Expression
        , operations :: List BinaryOperation
        }
    | UnaryOperator
        { operator :: AST.UnaryOperator
        , term :: Located Expression
        }
    | ListLiteral
        { terms :: List (Located Expression)
        }
    | TupleLiteral
        { terms :: List (Located Expression) -- At least two items
        }
    | RecordLiteral
        { base :: Maybe LowercaseIdentifier
        , fields :: Map LowercaseIdentifier (Located Expression) -- Cannot be empty if base is present
        , display_rl :: RecordDisplay
        }
    | RecordAccessFunction
        { field :: LowercaseIdentifier
        }
    | AnonymousFunction
        { parameters :: List (Located Pattern) -- Non-empty
        , body :: Located Expression
        }
    | IfExpression
        { if_ :: Located Expression
        , then_ :: Located Expression
        , else_ :: MaybeF Located Expression
        }
    | LetExpression
        { declarations :: List (Located LetDeclaration)
        , body :: Located Expression
        }
    | CaseExpression
        { subject :: Located Expression
        , branches :: List (Located CaseBranch)
        }
    | GLShader
        { shaderSource :: String
        }


data FunctionApplicationDisplay
    = FunctionApplicationDisplay
        { showAsRecordAccess :: Bool
        }

instance ToJSON FunctionApplicationDisplay where
    showJSON _ = \case
        FunctionApplicationDisplay showAsRecordAccess ->
            makeObj $ Maybe.catMaybes
                [ if showAsRecordAccess
                    then Just ( "showAsRecordAccess", JSBool True )
                    else Nothing
                ]


data MaybeF f a
    = JustF (f a)
    | NothingF a

instance (ToJSON a, ToJSON (f a)) => ToJSON (MaybeF f a) where
    showJSON c = \case
        JustF fa -> showJSON c fa
        NothingF a -> showJSON c a


instance ToPublicAST 'ExpressionNK where
    type PublicAST 'ExpressionNK = Expression

    fromRawAST' = \case
        AST.Unit comments ->
            UnitLiteral

        AST.Literal lit ->
            LiteralExpression lit

        AST.VarExpr var ->
            VariableReferenceExpression $ mkReference var

        AST.App expr args multiline ->
            FunctionApplication
                (JustF $ fromRawAST expr)
                (fmap (\(C comments a) -> fromRawAST a) args)
                (FunctionApplicationDisplay False)

        AST.Binops first rest multiline ->
            BinaryOperatorList
                (fromRawAST first)
                (fmap (\(AST.BinopsClause c1 op c2 expr) -> BinaryOperation (mkReference op) (fromRawAST expr)) rest)

        AST.Unary op expr ->
            UnaryOperator
                op
                (fromRawAST expr)

        AST.Parens (C comments expr) ->
            fromRawAST' $ extract $ I.unFix expr

        AST.ExplicitList terms comments multiline ->
            ListLiteral
                (fmap (\(C comments a) -> fromRawAST a) $ AST.toCommentedList terms)

        AST.Tuple terms multiline ->
            TupleLiteral
                (fmap (\(C comments a) -> fromRawAST a) terms)

        AST.TupleFunction n | n <= 1 ->
            error ("INVALID TUPLE CONSTRUCTOR: " ++ show n)

        AST.TupleFunction n ->
            VariableReferenceExpression
                (mkReference $ OpRef $ SymbolIdentifier $ replicate (n-1) ',')

        AST.Record base fields comments multiline ->
            RecordLiteral
                (fmap (\(C comments a) -> a) base)
                (Map.fromList $ fmap (\(C cp (Pair (C ck key) (C cv value) ml)) -> (key, fromRawAST value)) $ AST.toCommentedList fields)
                $ RecordDisplay
                    (fmap (extract . _key . extract) $ AST.toCommentedList fields)

        AST.Access base field ->
            FunctionApplication
                (NothingF $ RecordAccessFunction field)
                [ fromRawAST base ]
                (FunctionApplicationDisplay True)

        AST.Lambda parameters comments body multiline ->
            AnonymousFunction
                (fmap (\(C c a) -> fromRawAST a) parameters)
                (fromRawAST body)

        AST.If (AST.IfClause cond' thenBody') rest' (C c3 elseBody) ->
            ifThenElse cond' thenBody' rest'
            where
                ifThenElse (C c1 cond) (C c2 thenBody) rest =
                    IfExpression
                        (fromRawAST cond)
                        (fromRawAST thenBody)
                        $ case rest of
                            [] -> JustF $ fromRawAST elseBody
                            C c4 (AST.IfClause nextCond nextBody) : nextRest ->
                                NothingF $ ifThenElse nextCond nextBody nextRest

        AST.Let decls comments body ->
            LetExpression
                (fmap fromRawAST decls)
                (fromRawAST body)

        AST.Case (C comments subject, multiline) branches ->
            CaseExpression
                (fromRawAST subject)
                (fmap fromRawAST branches)

        AST.Range _ _ _ ->
            error "Range syntax is not supported in Elm 0.19"

        AST.AccessFunction field ->
            RecordAccessFunction field

        AST.GLShader shader ->
            GLShader shader


instance ToJSON Expression where
    showJSON c = \case
        UnitLiteral ->
            makeObj
                [ type_ "UnitLiteral"
                ]

        LiteralExpression lit ->
            showJSON c lit

        VariableReferenceExpression ref ->
            showJSON c ref

        FunctionApplication function arguments display ->
            makeObj $ mapMaybe removeEmpty
                [ type_ "FunctionApplication"
                , ( "function", showJSON c function )
                , ( "arguments", showJSON c arguments)
                , ( "display", showJSON c display )
                ]

        BinaryOperatorList first operations ->
            makeObj
                [ type_ "BinaryOperatorList"
                , ( "first", showJSON c first )
                , ( "operations", showJSON c operations )
                ]

        UnaryOperator operator term ->
            makeObj
                [ type_ "UnaryOperator"
                , ( "operator", showJSON c operator )
                , ( "term", showJSON c term )
                ]

        ListLiteral terms ->
            makeObj
                [ type_ "ListLiteral"
                , ( "terms", showJSON c terms)
                ]

        TupleLiteral terms ->
            makeObj
                [ type_ "TupleLiteral"
                , ( "terms", showJSON c terms)
                ]

        RecordLiteral Nothing fields display ->
            makeObj
                [ type_ "RecordLiteral"
                , ( "fields", showJSON c fields )
                , ( "display", showJSON c display )
                ]

        RecordLiteral (Just base) fields display ->
            makeObj
                [ type_ "RecordUpdate"
                , ( "base", showJSON c base )
                , ( "fields", showJSON c fields )
                , ( "display", showJSON c display )
                ]

        RecordAccessFunction field ->
            makeObj
                [ type_ "RecordAccessFunction"
                , ( "field", showJSON c field )
                ]

        AnonymousFunction parameters body ->
            makeObj
                [ type_ "AnonymousFunction"
                , ( "parameters", showJSON c parameters )
                , ( "body", showJSON c body )
                ]

        IfExpression if_ then_ else_ ->
            makeObj
                [ type_ "IfExpression"
                , ( "if", showJSON c if_ )
                , ( "then", showJSON c then_ )
                , ( "else", showJSON c else_ )
                ]

        LetExpression declarations body ->
            makeObj
                [ type_ "LetExpression"
                , ( "declarations", showJSON c declarations )
                , ( "body", showJSON c body )
                ]

        CaseExpression subject branches ->
            makeObj
                [ type_ "CaseExpression"
                , ( "subject", showJSON c subject )
                , ( "branches", showJSON c branches )
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
    fromRawAST' :: ASTNS1 Located [UppercaseIdentifier] nk -> PublicAST nk


fromRawAST :: ToPublicAST nk => ASTNS Located [UppercaseIdentifier] nk -> Located (PublicAST nk)
fromRawAST =
    fmap fromRawAST' . I.unFix


instance ToPublicAST 'PatternNK where
    type PublicAST 'PatternNK = Pattern

    fromRawAST' = \case
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
                (fmap (fromRawAST . (\(C comments a) -> a)) args)

        AST.PatternParens (C (pre, post) pat) ->
            extract $ fromRawAST pat

        AST.TuplePattern terms ->
            TuplePattern
                (fmap (fromRawAST . (\(C comments a) -> a)) terms)

        AST.EmptyListPattern comments ->
            mkListPattern [] Nothing

        AST.ListPattern terms ->
            mkListPattern
                (fmap (fromRawAST . (\(C comments a) -> a)) terms)
                Nothing

        AST.ConsPattern (C firstEol first) rest ->
            let
                first' = fromRawAST first
                rest' = fmap (fromRawAST . (\(C comments a) -> a)) (AST.toCommentedList rest)
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
                (fromRawAST pat)


instance ToPublicAST 'TypeNK where
    type PublicAST 'TypeNK = Type_

    fromRawAST' = \case
        AST.UnitType comments ->
            UnitType

        AST.TypeConstruction (AST.NamedConstructor (namespace, name)) args forceMultine ->
            TypeReference
                name
                (ModuleName namespace)
                (fmap (\(C comments a) -> fromRawAST a) args)

        AST.TypeVariable name ->
            TypeVariable name

        AST.TypeParens (C comments t) ->
            fromRawAST' (extract $ I.unFix t)

        AST.TupleType terms multiline ->
            TupleType
                (fmap (\(C comments a) -> fromRawAST a) terms)

        AST.RecordType base fields comments multiline ->
            RecordType
                (fmap (\(C comments a) -> a) base)
                (Map.fromList $ fmap (\(C cp (Pair (C ck key) (C cv value) ml)) -> (key, fromRawAST value)) $ AST.toCommentedList fields)
                $ RecordDisplay
                    (fmap (extract . _key . extract) $ AST.toCommentedList fields)

        AST.FunctionType first rest multiline ->
            case firstRestToRestLast first (AST.toCommentedList rest) of
                (args, C comments last) ->
                    FunctionType
                        (fromRawAST last)
                        (fmap (\(C comments a) -> fromRawAST a) args)
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
  showJSON :: Config -> a -> JSValue


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
    showJSON c = JSArray . fmap (showJSON c)


instance ToJSON a => ToJSON (Maybe a) where
    showJSON _ Nothing = JSNull
    showJSON c (Just a) = showJSON c a


instance (Show k, ToJSON v) => ToJSON (Map k v) where
    showJSON c =
        makeObj
            . fmap (\(k, a) -> (show k, showJSON c a))
            . Map.toList


instance ToJSON a => ToJSON (Located a) where
    showJSON c (A region a) =
        if showSourceLocation c
            then addField ( "sourceLocation", showJSON c region ) (showJSON c a)
            else showJSON c a


instance ToJSON Region where
    showJSON c region =
        makeObj
            [ ( "start", showJSON c $ Region.start region )
            , ( "end", showJSON c $ Region.end region )
            ]


instance ToJSON Region.Position where
    showJSON _ pos =
        makeObj
            [ ( "line", JSRational False $ toRational $ Region.line pos )
            , ( "col", JSRational False $ toRational $ Region.column pos )
            ]


instance ToJSON UppercaseIdentifier where
    showJSON _ (UppercaseIdentifier name) = JSString $ toJSString name


instance ToJSON LowercaseIdentifier where
    showJSON _ (LowercaseIdentifier name) = JSString $ toJSString name


instance ToJSON SymbolIdentifier where
    showJSON _ (SymbolIdentifier sym) = JSString $ toJSString sym


instance ToJSON (Ref ()) where
    showJSON c (VarRef () var) = showJSON c var
    showJSON c (TagRef () tag) = showJSON c tag
    showJSON c (OpRef sym) = showJSON c sym


instance ToJSON AST.UnaryOperator where
    showJSON _ Negative = JSString $ toJSString "-"


instance ToJSON (AST.Listing AST.DetailedListing) where
    showJSON c = \case
        AST.ExplicitListing a comments -> showJSON c a
        AST.OpenListing (C comments ()) -> JSString $ toJSString "Everything"
        AST.ClosedListing -> JSNull


instance ToJSON AST.DetailedListing where
    showJSON _ = \case
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
    showJSON _ DecimalInt = JSString $ toJSString "DecimalInt"
    showJSON _ HexadecimalInt = JSString $ toJSString "HexadecimalInt"


instance ToJSON FloatRepresentation where
    showJSON _ DecimalFloat = JSString $ toJSString "DecimalFloat"
    showJSON _ ExponentFloat = JSString $ toJSString "ExponentFloat"


instance ToJSON StringRepresentation where
    showJSON _ SingleQuotedString = JSString $ toJSString "SingleQuotedString"
    showJSON _ TripleQuotedString = JSString $ toJSString "TripleQuotedString"


instance ToJSON ModuleName where
    showJSON _ (ModuleName []) = JSNull
    showJSON _ namespace = JSString $ toJSString $ show namespace


instance ToJSON AST.LiteralValue where
    showJSON c = \case
        IntNum value repr ->
            makeObj
                [ type_ "IntLiteral"
                , ("value", JSRational False $ toRational value)
                , ("display"
                , makeObj
                    [ ("representation", showJSON c repr)
                    ]
                )
                ]

        FloatNum value repr ->
            makeObj
                [ type_ "FloatLiteral"
                , ("value", JSRational False $ toRational value)
                , ("display"
                , makeObj
                    [ ("representation", showJSON c repr)
                    ]
                )
                ]

        Boolean value ->
            showJSON c $
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
                    [ ( "representation", showJSON c repr )
                    ]
                )
                ]


instance ToJSON Pattern where
    showJSON c = \case
        AnythingPattern ->
            makeObj
                [ type_ "AnythingPattern"
                ]

        UnitPattern ->
            makeObj
                [ type_ "UnitPattern"
                ]

        LiteralPattern lit ->
            showJSON c lit

        VariablePattern def ->
            showJSON c def

        DataPattern constructor arguments ->
            makeObj
                [ type_ "DataPattern"
                , ( "constructor", showJSON c constructor )
                , ( "arguments", showJSON c arguments )
                ]

        TuplePattern terms ->
            makeObj
                [ type_ "TuplePattern"
                , ( "terms", showJSON c terms )
                ]

        ListPattern prefix rest ->
            makeObj
                [ type_ "ListPattern"
                , ( "prefix", showJSON c prefix )
                , ( "rest", showJSON c rest )
                ]

        RecordPattern fields ->
            makeObj
                [ type_ "RecordPattern"
                , ( "fields", showJSON c fields )
                ]

        PatternAlias alias pat ->
            makeObj
                [ type_ "PatternAlias"
                , ( "alias", showJSON c alias )
                , ( "pattern", showJSON c pat )
                ]


instance ToJSON Type_ where
    showJSON c = \case
        UnitType ->
            makeObj
                [ type_ "UnitType"
                ]

        TypeReference name module_ arguments ->
            makeObj
                [ type_ "TypeReference"
                , ( "name", showJSON c name )
                , ( "module", showJSON c module_ )
                , ( "arguments", showJSON c arguments )
                ]

        TypeVariable name ->
            makeObj
                [ type_ "TypeVariable"
                , ( "name", showJSON c name )
                ]

        TupleType terms ->
            makeObj
                [ type_ "TupleType"
                , ( "terms", showJSON c terms )
                ]

        RecordType Nothing fields display ->
            makeObj
                [ type_ "RecordType"
                , ( "fields", showJSON c fields )
                , ( "display", showJSON c display )
                ]

        RecordType (Just base) fields display ->
            makeObj
                [ type_ "RecordTypeExtension"
                , ( "base", showJSON c base )
                , ( "fields", showJSON c fields )
                , ( "display", showJSON c display )
                ]

        FunctionType returnType argumentTypes ->
            makeObj
                [ type_ "FunctionType"
                , ( "returnType", showJSON c returnType )
                , ( "argumentTypes", showJSON c argumentTypes )
                ]


instance ToJSON RecordDisplay where
    showJSON c (RecordDisplay fieldOrder) =
        makeObj
            [ ( "fieldOrder", showJSON c fieldOrder )
            ]


instance ToJSON Reference where
    showJSON c = \case
        ExternalReference module_ identifier ->
            makeObj
                [ type_ "ExternalReference"
                , ("module", showJSON c module_)
                , ("identifier", showJSON c identifier)
                ]

        VariableReference name ->
            makeObj
                [ type_ "VariableReference"
                , ( "name", showJSON c name )
                ]


instance ToJSON VariableDefinition where
    showJSON c (VariableDefinition name) =
        makeObj
            [ type_ "VariableDefinition"
            , ( "name" , showJSON c name )
            ]
