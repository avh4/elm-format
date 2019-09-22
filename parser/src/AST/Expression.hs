{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module AST.Expression where

import AST.V0_16
import Data.Fix

import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


---- GENERAL AST ----

data UnaryOperator =
    Negative
    deriving (Eq, Show)


data LetDeclaration e
  = LetDefinition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments e
  | LetAnnotation (Var.Ref, Comments) (Comments, Type)
  | LetComment Comment
  deriving (Eq, Show)


instance Functor LetDeclaration where
    fmap f (LetDefinition n args c e) = LetDefinition n args c (f e)
    fmap _ (LetAnnotation n t) = LetAnnotation n t
    fmap _ (LetComment c) = LetComment c


type Expr =
    Fix LocatedExpression

data IfClause e =
    IfClause (Commented e) (Commented e)
    deriving (Eq, Show)

instance Functor IfClause where
    fmap f (IfClause a b) = IfClause (fmap f a) (fmap f b)


data BinopsClause e =
    BinopsClause Comments Var.Ref Comments e
    deriving (Eq, Show)

instance Functor BinopsClause where
    fmap f (BinopsClause a b c e) = BinopsClause a b c (f e)


type Expr' =
    Expression Expr


newtype LocatedExpression e =
    LocatedExpression (A.Located (Expression e))
    deriving (Eq, Show)


instance Functor LocatedExpression where
    fmap f (LocatedExpression e) = LocatedExpression $ fmap (fmap f) e


data Expression e
    = Unit Comments
    | Literal Literal
    | VarExpr Var.Ref

    | App e [(Comments, e)] FunctionApplicationMultiline
    | Unary UnaryOperator e
    | Binops e [BinopsClause e] Bool
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

    | Lambda [(Comments, Pattern.Pattern)] Comments e Bool
    | If (IfClause e) [(Comments, IfClause e)] (Comments, e)
    | Let [LetDeclaration e] Comments e
    | Case (Commented e, Bool) [(Commented Pattern.Pattern, (Comments, e))]

    -- for type checking and code gen only
    | GLShader String
    deriving (Eq, Show)


instance Functor Expression where
    fmap _ (Unit c) = Unit c
    fmap _ (Literal l) = Literal l
    fmap _ (VarExpr v) = VarExpr v
    fmap f (App e args m) = App (f e) (fmap (fmap f) args) m
    fmap f (Unary op e) = Unary op (f e)
    fmap f (Binops e rest b) = Binops (f e) (fmap (fmap f) rest) b
    fmap f (Parens e) = Parens (fmap f e)
    fmap f (ExplicitList terms c m) = ExplicitList (fmap (fmap $ fmap $ fmap f) terms) c m
    fmap f (Range a b m) = Range (fmap f a) (fmap f b) m
    fmap f (Tuple es m) = Tuple (fmap (fmap f) es) m
    fmap _ (TupleFunction n) = TupleFunction n
    fmap f (Record base fields c m) = Record base (fmap (fmap $ fmap $ fmap $ fmap f) fields) c m
    fmap f (Access e n) = Access (f e) n
    fmap _ (AccessFunction n) = AccessFunction n
    fmap f (Lambda args c e m) = Lambda args c (f e) m
    fmap f (If first rest els) = If (fmap f first) (fmap (fmap $ fmap f) rest) (fmap f els)
    fmap f (Let defs c e) = Let (fmap (fmap f) defs) c (f e)
    fmap f (Case (e, m) bs) = Case (fmap f e, m) (fmap (fmap $ fmap f) bs)
    fmap _ (GLShader s) = GLShader s
