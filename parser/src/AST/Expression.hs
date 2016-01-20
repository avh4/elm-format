{-# OPTIONS_GHC -Wall #-}

module AST.Expression where

import AST.V0_16
import qualified AST.Pattern as Pattern
import qualified AST.Variable as Var
import qualified AST.GLShader as L
import qualified Reporting.Annotation as A


---- GENERAL AST ----

data UnaryOperator =
    Negative
    deriving (Eq, Show)


data LetDeclaration
  = LetDefinition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments Expr Bool
  | LetAnnotation (Var.Ref, Comments) (Comments, Type)
  | LetComment Comment
  deriving (Eq, Show)


type Expr =
    A.Located Expr'


type IfClause =
  (Commented Expr, Commented Expr)


data Expr'
    = Unit Comments
    | Literal Literal
    | Var Var.Ref

    | App Expr [(Comments, Expr)] Bool
    | Unary UnaryOperator Expr
    | Binops Expr [(Comments, Var.Ref, Comments, Expr)] Bool
    | Parens (Commented Expr)

    | EmptyList Comments
    | ExplicitList [Commented Expr] Bool
    | Range (Commented Expr) (Commented Expr) Bool

    | Tuple [Commented Expr] Bool
    | TupleFunction Int -- will be 2 or greater, indicating the number of elements in the tuple

    | EmptyRecord Comments
    | Record [(Commented String, Commented Expr, Bool)] Bool
    | RecordUpdate (Commented Expr) [(Commented String, Commented Expr, Bool)] Bool
    | Access Expr String
    | AccessFunction String

    | Lambda [(Comments, Pattern.Pattern)] Comments Expr Bool
    | If IfClause [(Comments, IfClause)] (Comments, Expr)
    | Let [LetDeclaration] Comments Expr
    | Case (Commented Expr, Bool) [(Commented Pattern.Pattern, (Comments, Expr))]

    -- for type checking and code gen only
    | GLShader String String L.GLShaderTipe
    deriving (Eq, Show)
