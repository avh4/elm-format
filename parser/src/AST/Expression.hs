{-# OPTIONS_GHC -Wall #-}

module AST.Expression where

import AST.V0_16
import qualified AST.Pattern as Pattern
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified AST.GLShader as L
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
    = Unit [Comment]
    | Literal Literal
    | Var Var.Ref

    | App Expr [([Comment], Expr)] Bool
    | Unary UnaryOperator Expr
    | Binops Expr [([Comment], Var.Ref, [Comment], Expr)] Bool
    | Parens (Commented Expr)

    | ExplicitList [Commented Expr] Bool
    | Range (Commented Expr) (Commented Expr) Bool

    | Tuple [Commented Expr] Bool
    | TupleFunction Int -- will be 2 or greater, indicating the number of elements in the tuple

    | EmptyRecord [Comment]
    | Record [([Comment], String, [Comment], Commented Expr, Bool)] Bool
    | RecordUpdate (Commented Expr) [([Comment], String, [Comment], Commented Expr, Bool)] Bool
    | Access Expr String
    | AccessFunction String

    | Lambda [([Comment], Pattern.Pattern)] [Comment] Expr Bool
    | If [(Expr, Bool, [Comment], Expr)] [Comment] Expr
    | Let [Def] [Comment] Expr
    | Case (Expr,Bool) [([Comment], Pattern.Pattern, [Comment], Expr)]

    -- for type checking and code gen only
    | GLShader String String L.GLShaderTipe
    deriving (Eq, Show)
