module Parse.Binop (binops) where

import Control.Applicative ((<$>))
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Parsec ((<|>), choice, getState, try)

import AST.Declaration (Assoc(L, N, R))
import AST.Expression (Expr'(Binop))
import qualified AST.Expression as E
import qualified AST.Variable as Var
import Parse.Helpers (IParser, commitIf, failure, whitespace)
import qualified Parse.OpTable as OpTable
import qualified Parse.State as State
import qualified Reporting.Annotation as A


binops
    :: IParser E.Expr
    -> IParser E.Expr
    -> IParser String
    -> IParser E.Expr
binops term last anyOp =
  do  e <- term
      state <- getState
      split (State.ops state) 0 e =<< nextOps
  where
    nextOps =
      choice
        [ commitIf (whitespace >> anyOp) $
            do  whitespace
                op <- anyOp
                whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (op,t) <$> nextOps
                  Right e -> return [(op,e)]
        , return []
        ]


split
    :: OpTable.OpTable
    -> Int
    -> E.Expr
    -> [(String, E.Expr)]
    -> IParser E.Expr
split _ _ e [] = return e
split table n e eops =
  do  assoc <- getAssoc table n eops
      es <- sequence (splitLevel table n e eops)
      let ops = map fst (filter (OpTable.hasLevel table n) eops)
      case assoc of
        R -> joinR es ops
        _ -> joinL es ops


splitLevel
    :: OpTable.OpTable
    -> Int
    -> E.Expr
    -> [(String, E.Expr)]
    -> [IParser E.Expr]
splitLevel table n e eops =
  case break (OpTable.hasLevel table n) eops of
    (lops, (_op,e'):rops) ->
        split table (n+1) e lops : splitLevel table n e' rops

    (lops, []) ->
        [ split table (n+1) e lops ]


joinL :: [E.Expr] -> [String] -> IParser E.Expr
joinL exprs ops =
  case (exprs, ops) of
    ([expr], []) ->
        return expr

    (a:b:remainingExprs, op:remainingOps) ->
        let binop = A.merge a b (Binop (Var.OpRef op) a b)
        in
            joinL (binop : remainingExprs) remainingOps

    (_, _) ->
        failure "Ill-formed binary expression. Report a compiler bug."


joinR :: [E.Expr] -> [String] -> IParser E.Expr
joinR exprs ops =
  case (exprs, ops) of
    ([expr], []) ->
        return expr

    (a:b:remainingExprs, op:remainingOps) ->
        do  e <- joinR (b:remainingExprs) remainingOps
            return (A.merge a e (Binop (Var.OpRef op) a e))

    (_, _) ->
        failure "Ill-formed binary expression. Report a compiler bug."


getAssoc :: OpTable.OpTable -> Int -> [(String,E.Expr)] -> IParser Assoc
getAssoc table n eops
    | all (==L) assocs = return L
    | all (==R) assocs = return R
    | all (==N) assocs =
        case assocs of
          [_] -> return N
          _   -> failure (msg "precedence")
    | otherwise = failure (msg "associativity")
  where
    levelOps = filter (OpTable.hasLevel table n) eops
    assocs = map (OpTable.assoc table . fst) levelOps
    msg problem =
        concat
          [ "Conflicting " ++ problem ++ " for binary operators ("
          , List.intercalate ", " (map fst eops), "). "
          , "Consider adding parentheses to disambiguate."
          ]
