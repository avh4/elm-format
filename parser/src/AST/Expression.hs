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

type Def =
    A.Located Def'


data Def'
    = Definition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments Expr Bool
    | TypeAnnotation Var.Ref Type
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
    | If [(Commented Expr, Bool, Comments, Expr)] Comments Expr
    | Let [Def] Comments Expr
    | Case (Expr,Bool) [(Comments, Pattern.Pattern, Comments, Expr)]

    -- for type checking and code gen only
    | GLShader String String L.GLShaderTipe
    deriving (Eq, Show)
