{-# OPTIONS_GHC -Wall #-}

module AST.Expression where

import AST.V0_15
import qualified AST.Literal as Literal
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


---- GENERAL AST ----

data UnaryOperator =
    Negative
    deriving (Eq, Show)

type Def =
    A.Located Def'


data Def'
    = Definition Pattern.Pattern [Pattern.Pattern] [Comment] Expr Bool
    | TypeAnnotation Var.Ref Type.Type
    deriving (Eq, Show)


stripRegion :: Def -> Def
stripRegion d =
    A.stripRegion $ A.map stripRegion' d


stripRegion' :: Def' -> Def'
stripRegion' d =
    case d of
        Definition p ps c e b ->
            Definition (A.stripRegion p) (map A.stripRegion ps) c (A.stripRegion e) b
        _ ->
            d


type Expr =
    A.Located Expr'


data Expr'
    = Literal Literal.Literal
    | Var Var.Ref
    | Range (Commented Expr) (Commented Expr) Bool
    | ExplicitList [Expr] Bool
    | Binops Expr [(Commented Var.Ref, Commented Expr)] Bool -- will only have pre comments
    | Unary UnaryOperator Expr
    | Lambda [Pattern.Pattern] Expr Bool
    | App Expr [Commented Expr] Bool -- will only have pre comments
    | If [(Expr, Bool, Expr)] Expr
    | Let [Def] Expr
    | Case (Expr,Bool) [(Pattern.Pattern, Expr)]
    | Tuple [Commented Expr] Bool
    | TupleFunction Int -- will be 2 (,) or greater, indicating the size of the tuple
    | Access Expr String
    | AccessFunction String
    | RecordUpdate Expr [(String, Expr, Bool)] Bool
    | Record [(String, Expr, Bool)] Bool
    | Parens Expr Bool
    | Unit
    -- for type checking and code gen only
    | GLShader String String Literal.GLShaderTipe
    deriving (Eq, Show)
