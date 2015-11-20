{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import AST.V0_15
import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


type Pattern =
    A.Located Pattern'


data Pattern'
    = Data Var.Ref [Pattern]
    | Tuple [Pattern]
    | Record [String]
    | Alias String Pattern
    | Var Var.Ref
    | Anything
    | Literal L.Literal
    deriving (Eq, Show)


list :: R.Position -> [Pattern] -> Pattern
list end patterns =
  case patterns of
    [] ->
        A.at end end (Data (Var.OpRef "[]") [])

    pattern@(A.A (R.Region start _) _) : rest ->
        A.at start end (Data (Var.OpRef "::") [pattern, list end rest])


consMany :: R.Position -> [Pattern] -> Pattern
consMany end patterns =
  let cons hd@(A.A (R.Region start _) _) tl =
          A.at start end (Data (Var.OpRef "::") [hd, tl])
  in
      foldr1 cons patterns


tuple :: [Pattern] -> Pattern'
tuple patterns =
  Tuple patterns
