module Parse.OpTable where

import qualified AST.Declaration
import qualified AST.Variable as Var
import qualified Data.Map as Map


type OpTable = Map.Map Var.Ref (Int, AST.Declaration.Assoc)


empty :: OpTable
empty =
	Map.empty


level :: OpTable -> Var.Ref -> Int
level table op =
  fst $ Map.findWithDefault (9,AST.Declaration.L) op table


assoc :: OpTable -> Var.Ref -> AST.Declaration.Assoc
assoc table op =
  snd $ Map.findWithDefault (9,AST.Declaration.L) op table


hasLevel :: OpTable -> Int -> (Var.Ref, a) -> Bool
hasLevel table n (op,_) =
  level table op == n
