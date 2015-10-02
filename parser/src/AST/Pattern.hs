{-# OPTIONS_GHC -Wall #-}
module AST.Pattern where

import qualified Data.Set as Set

import qualified AST.Literal as L
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A
import qualified Reporting.Region as R


type Pattern =
    A.Annotated R.Region Pattern'


data Pattern'
    = Data Var.Ref [Pattern]
    | Tuple [Pattern]
    | Record [String]
    | Alias String Pattern
    | Var Var.Ref
    | Anything
    | Literal L.Literal
    deriving (Show)


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


-- FIND VARIABLES

boundVars :: Pattern -> [A.Annotated R.Region Var.Ref]
boundVars (A.A ann pattern) =
  case pattern of
    Var x ->
        [A.A ann x]

    Alias name realPattern ->
        A.A ann (Var.VarRef name) : boundVars realPattern

    Data _ patterns ->
        concatMap boundVars patterns

    Tuple patterns ->
        concatMap boundVars patterns

    Record fields ->
        map (A.A ann . Var.VarRef) fields

    Anything ->
        []

    Literal _ ->
        []


member :: Var.Ref -> Pattern -> Bool
member name pattern =
  any (name==) (map A.drop (boundVars pattern))


boundVarSet :: Pattern -> Set.Set Var.Ref
boundVarSet pattern =
  Set.fromList (map A.drop (boundVars pattern))


boundVarList :: Pattern -> [Var.Ref]
boundVarList pattern =
  Set.toList (boundVarSet pattern)
