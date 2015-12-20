{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import AST.V0_16
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


type Pattern =
    A.Located Pattern'


data Pattern'
    = Anything
    | Literal Literal
    | Var Var.Ref
    | Data Var.Ref [Pattern]
    | Tuple [Pattern]
    | List [Pattern]
    | Record [String]
    | Alias String Pattern
    deriving (Eq, Show)


consMany :: R.Position -> [Pattern] -> Pattern
consMany end patterns =
  let cons hd@(A.A (R.Region start _) _) tl =
          A.at start end (Data (Var.OpRef "::") [hd, tl])
  in
      foldr1 cons patterns
