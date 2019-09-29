{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module AST.Expression where

import AST.V0_16
import Data.Fix
import ElmFormat.Mapping

import AST.Pattern (Pattern)
import qualified AST.Variable as Var
import qualified Data.Map.Strict as Dict
import qualified Data.Maybe as Maybe
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


---- GENERAL AST ----

data UnaryOperator =
    Negative
    deriving (Eq, Show)


data LetDeclaration ns e
  = LetDefinition (Pattern ns) [(Comments, (Pattern ns))] Comments e
  | LetAnnotation (Var.Ref (), Comments) (Comments, Type ns)
  | LetComment Comment
  deriving (Eq, Show, Functor)

instance MapNamespace a b (LetDeclaration a e) (LetDeclaration b e) where
    mapNamespace f ld =
        case ld of
            LetDefinition first rest comments body -> LetDefinition (fmap (fmap f) first) (fmap (fmap $ fmap $ fmap f) rest) comments body
            LetAnnotation name typ -> LetAnnotation name (fmap (fmap $ fmap f) typ)
            LetComment comment -> LetComment comment


type Expr =
    Fix (AnnotatedExpression [UppercaseIdentifier] R.Region)

data IfClause e =
    IfClause (Commented e) (Commented e)
    deriving (Eq, Show, Functor)


data BinopsClause ns e =
    BinopsClause Comments (Var.Ref ns) Comments e
    deriving (Eq, Show, Functor)


instance MapNamespace a b (BinopsClause a e) (BinopsClause b e) where
    mapNamespace f (BinopsClause pre op post e) =
        BinopsClause pre (fmap f op) post e


type Expr' =
    Expression [UppercaseIdentifier] Expr


newtype AnnotatedExpression ns ann e =
    AE (A.Annotated ann (Expression ns e))
    deriving (Eq, Show, Functor)


instance MapNamespace a b (AnnotatedExpression a ann e) (AnnotatedExpression b ann e)
  where
    mapNamespace f (AE (A.A ann e)) = AE $ A.A ann $ mapNamespace f e


instance MapNamespace a b (Fix (AnnotatedExpression a ann)) (Fix (AnnotatedExpression b ann)) where
    mapNamespace f = cata (Fix . mapNamespace f)


stripAnnotation :: Fix (AnnotatedExpression ns ann) -> Fix (Expression ns)
stripAnnotation (Fix (AE (A.A _ e))) = Fix $ fmap stripAnnotation e


dropAnnotation :: Fix (AnnotatedExpression ns ann) -> Expression ns (Fix (AnnotatedExpression ns ann))
dropAnnotation (Fix (AE (A.A _ e))) = e


mapAnnotation :: (a -> b) -> Fix (AnnotatedExpression ns a) -> Fix (AnnotatedExpression ns b)
mapAnnotation f (Fix (AE (A.A a e))) = Fix $ AE $ A.A (f a) $ fmap (mapAnnotation f) e


addAnnotation :: ann -> Fix (Expression ns) -> Fix (AnnotatedExpression ns ann)
addAnnotation ann (Fix e) = Fix $ AE $ A.A ann $ fmap (addAnnotation ann) e


instance MapNamespace a b (Fix (Expression a)) (Fix (Expression b)) where
    mapNamespace f = cata (Fix . mapNamespace f)


data Expression ns e
    = Unit Comments
    | Literal Literal
    | VarExpr (Var.Ref ns)

    | App e [(Comments, e)] FunctionApplicationMultiline
    | Unary UnaryOperator e
    | Binops e [BinopsClause ns e] Bool
    | Parens (Commented e)

    | ExplicitList
        { terms :: Sequence e
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | Range (Commented e) (Commented e) Bool

    | Tuple [Commented e] Bool
    | TupleFunction Int -- will be 2 or greater, indicating the number of elements in the tuple

    | Record
        { base :: Maybe (Commented LowercaseIdentifier)
        , fields :: Sequence (Pair LowercaseIdentifier e)
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | Access e LowercaseIdentifier
    | AccessFunction LowercaseIdentifier

    | Lambda [(Comments, Pattern ns)] Comments e Bool
    | If (IfClause e) [(Comments, IfClause e)] (Comments, e)
    | Let [LetDeclaration ns e] Comments e
    | Case (Commented e, Bool) [(Commented (Pattern ns), (Comments, e))]

    -- for type checking and code gen only
    | GLShader String
    deriving (Eq, Show, Functor)


instance MapNamespace a b (Expression a e) (Expression b e) where
    mapNamespace f expr =
        case expr of
            VarExpr var -> VarExpr (fmap f var)
            Binops left restOps multiline ->
                Binops left (fmap (mapNamespace f) restOps) multiline
            Let decls pre body ->
                Let (fmap (mapNamespace f) decls) pre body

            Unit c ->
                Unit c
            AST.Expression.Literal l ->
                AST.Expression.Literal l
            App f' args multiline ->
                App f' args multiline
            Unary op e ->
                Unary op e
            Parens e ->
                Parens e
            ExplicitList terms' post multiline ->
                ExplicitList terms' post multiline
            Range e1 e2 multiline ->
                Range e1 e2 multiline
            AST.Expression.Tuple es multiline ->
                AST.Expression.Tuple es multiline
            TupleFunction n ->
                TupleFunction n
            AST.Expression.Record b fs post multiline ->
                AST.Expression.Record b fs post multiline
            Access e field' ->
                Access e field'
            AccessFunction n ->
                AccessFunction n
            Lambda params pre body multi ->
                Lambda (fmap (fmap $ fmap $ fmap f) params) pre body multi
            If c1 elseIfs els ->
                If c1 elseIfs els
            Case (cond, m) branches ->
                Case (cond, m) (fmap (mapFst $ fmap $ fmap $ fmap f) branches)
            GLShader s ->
                GLShader s
        where
            mapFst f (a, x) = (f a, x)


bottomUp :: Functor f => (Fix f -> Fix f) -> Fix f -> Fix f
bottomUp f = cata (f . Fix)


type UsageCount ns = Dict.Map ns (Dict.Map String Int)


countUsages :: Ord ns => Expression ns (UsageCount ns) -> UsageCount ns
countUsages e' =
    let
        mergeUsage = Dict.unionsWith (Dict.unionWith (+))
        _sequence = fmap (\(_, (_, WithEol t _)) -> t)
        _pair (Pair (k, _) (_, v) _) = (k, v)
        _c (Commented _ e _) = e
        _letDeclaration l =
            case l of
                LetDefinition _ _ _ e -> Just e
                _ -> Nothing
        countIfClause (IfClause a b) = mergeUsage [_c a, _c b]
    in
    case e' of
        Unit _ -> Dict.empty
        Literal _ -> Dict.empty
        VarExpr (Var.VarRef ns (LowercaseIdentifier n)) -> Dict.singleton ns (Dict.singleton n 1)
        VarExpr (Var.TagRef ns (UppercaseIdentifier n)) -> Dict.singleton ns (Dict.singleton n 1)
        VarExpr (Var.OpRef _) -> Dict.empty
        App e args _ -> mergeUsage (e : fmap snd args)
        Unary _ e -> e
        Binops e ops _ -> mergeUsage (e : fmap (\(BinopsClause _ _ _ t) -> t) ops)
        Parens e -> _c e
        ExplicitList terms _ _ -> mergeUsage (_sequence terms)
        Range a b _ -> mergeUsage [_c a, _c b]
        Tuple terms _ -> mergeUsage (fmap _c terms)
        TupleFunction _ -> Dict.empty
        Record _ fields _ _ -> mergeUsage (fmap (snd . _pair) $ _sequence fields)
        Access e _ -> e
        AccessFunction _ -> Dict.empty
        Lambda _ _ e _ -> e
        If a rest (_, e) -> mergeUsage (countIfClause a : e : fmap (countIfClause . snd) rest)
        Let defs _ e -> mergeUsage (e : Maybe.mapMaybe _letDeclaration defs)
        Case (e, _) branches -> mergeUsage (_c e : fmap (snd . snd) branches)
        GLShader _ -> Dict.empty
