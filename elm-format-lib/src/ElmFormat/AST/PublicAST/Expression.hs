{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ElmFormat.AST.PublicAST.Expression (Expression(..), Definition(..), DefinitionBuilder(..), TypedParameter(..), mkDefinitions) where

import ElmFormat.AST.PublicAST.Core
import ElmFormat.AST.PublicAST.Reference
import qualified AST.V0_16 as AST
import qualified Data.Indexed as I
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified ElmFormat.AST.PatternMatching as PatternMatching
import qualified Data.Maybe as Maybe
import ElmFormat.AST.PublicAST.Pattern
import ElmFormat.AST.PublicAST.Type
import ElmFormat.AST.PublicAST.Comment
import Data.Maybe (mapMaybe)
import Data.Text (Text)


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
                ((\(C comments a) -> fromRawAST config a) <$> AST.toCommentedList terms)

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
                (Map.fromList $ (\(C cp (Pair (C ck key) (C cv value) ml)) -> (key, fromRawAST config value)) <$> AST.toCommentedList fields)
                $ RecordDisplay
                    (extract . _key . extract <$> AST.toCommentedList fields)

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

        LiteralExpression lit ->
            AST.Literal lit

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
                , pair "display" <$> toMaybeEncoding display
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
        tag :: Text <- obj .: "tag"
        case tag of
            "UnitLiteral" ->
                return UnitLiteral

            _ ->
                return $ LiteralExpression $ Str ("TODO: " <> show (Object obj)) SingleQuotedString


newtype FunctionApplicationDisplay
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



--
-- Definition
--


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

instance FromJSON TypedParameter where
    parseJSON = withObject "TypedParameter" $ \obj ->
        TypedParameter
            <$> obj .: "pattern"
            <*> return Nothing


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
                        ( fmap (, Nothing) args, Nothing )
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

        annotations :: Map LowercaseIdentifier (AST.C2 'AST.BeforeSeparator 'AST.AfterSeparator (ASTNS Located [UppercaseIdentifier] 'TypeNK))
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
    mapMaybe (traverse merge) items

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
                    <*> obj .: "parameters"
                    <*> return Nothing -- TODO
                    <*> obj .: "expression"

            _ ->
                fail ("unexpected Definition tag: " <> tag)

