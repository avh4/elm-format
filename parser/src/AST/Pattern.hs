{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import AST.V0_16
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


type Pattern =
    A.Located Pattern'


data Pattern'
    = Anything
    | UnitPattern Comments
    | Literal Literal
    | Var Var.Ref
    | Data String [(Comments, Pattern)]
    | Tuple [Commented Pattern]
    | EmptyListPattern Comments
    | List [Commented Pattern]
    | ConsPattern Pattern [Pattern] Pattern
    | Record [Commented String]
    | Alias (Pattern, Comments) (Comments, String)
    deriving (Eq, Show)
