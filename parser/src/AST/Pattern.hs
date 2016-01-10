{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import AST.V0_16
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


type Pattern =
    A.Located Pattern'


data Pattern'
    = Anything
    | UnitPattern [Comment]
    | Literal Literal
    | Var Var.Ref
    | Data String [Pattern]
    | Tuple [Commented Pattern]
    | EmptyListPattern [Comment]
    | List [Commented Pattern]
    | ConsPattern Pattern [Pattern] Pattern
    | Record [Commented String]
    | Alias (Pattern, [Comment]) ([Comment], String)
    deriving (Eq, Show)
