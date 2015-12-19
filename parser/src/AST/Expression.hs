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
    = Definition Pattern.Pattern [([Comment], Pattern.Pattern)] [Comment] Expr Bool
    | TypeAnnotation Var.Ref Type.Type
    | LetComment Comment
    deriving (Eq, Show)


stripRegion :: Def -> Def
stripRegion d =
    A.stripRegion $ A.map stripRegion' d


stripRegion' :: Def' -> Def'
stripRegion' d =
    case d of
        Definition p ps c e b ->
            Definition (A.stripRegion p) (map (\(a,b') -> (a,A.stripRegion b')) ps) c (A.stripRegion e) b
        _ ->
            d


type Expr =
    A.Located Expr'


data Expr'
    = Literal Literal.Literal
    | Var Var.Ref
    | Range (Commented Expr) (Commented Expr) Bool
    | ExplicitList [Commented Expr] Bool
    | Binops Expr [([Comment], Var.Ref, [Comment], Expr)] Bool
    | Unary UnaryOperator Expr
    | Lambda [([Comment], Pattern.Pattern)] [Comment] Expr Bool
    | App Expr [([Comment], Expr)] Bool
    | If [(Expr, Bool, [Comment], Expr)] [Comment] Expr
    | Let [Def] [Comment] Expr
    | Case (Expr,Bool) [([Comment], Pattern.Pattern, [Comment], Expr)]
    | Tuple [Commented Expr] Bool
    | TupleFunction Int -- will be 2 (,) or greater, indicating the size of the tuple
    | Access Expr String
    | AccessFunction String
    | RecordUpdate (Commented Expr) [([Comment], String, [Comment], Commented Expr, Bool)] Bool
    | Record [([Comment], String, [Comment], Commented Expr, Bool)] Bool
    | EmptyRecord [Comment]
    | Parens (Commented Expr)
    | Unit [Comment]
    -- for type checking and code gen only
    | GLShader String String Literal.GLShaderTipe
    deriving (Eq, Show)
