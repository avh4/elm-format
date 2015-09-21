module Parse.OpTable where

import qualified AST.Declaration
import qualified Data.Map as Map


type OpTable = Map.Map String (Int, AST.Declaration.Assoc)


empty :: OpTable
empty =
	Map.empty


level :: OpTable -> String -> Int
level table op =
  fst $ Map.findWithDefault (9,AST.Declaration.L) op table


assoc :: OpTable -> String -> AST.Declaration.Assoc
assoc table op =
  snd $ Map.findWithDefault (9,AST.Declaration.L) op table


hasLevel :: OpTable -> Int -> (String, a) -> Bool
hasLevel table n (op,_) =
  level table op == n
