module ElmFormat.Render.GroupBinops (extractAnds) where

import AST.V0_16
import AST.Expression
import Reporting.Annotation
import qualified AST.Variable as Var
import qualified Reporting.Region as Region


-- TODO: handle Ors also
-- TODO: ensure it only groups comparison operators?
-- TODO: make it work for expression that aren't directly the body of a definition
extractAnds :: Expr -> Expr
extractAnds expr =
    case expr of
        A loc (Binops left ops multiline) ->
            A loc $ done multiline $ foldl step (left, [], []) ops

        _ ->
            expr


type State = (Expr, [(Comments, Var.Ref, Comments, Expr)], [(Expr, Var.Ref)])

shiftReverse :: [(a, b)] -> a -> [(b, a)] -> (a, [(b, a)])
shiftReverse [] last acc = (last, acc)
shiftReverse ((prev, op):rest) last acc =
    shiftReverse rest prev ((op, last) : acc)


step :: State -> (Comments, Var.Ref, Comments, Expr) -> State
step (left, inners, conditions) (pre, op, post, next) =  -- TODO: Handle comments
  case op of
    Var.OpRef (SymbolIdentifier opSymbol) ->
      if opSymbol == "&&" || opSymbol == "<|" then
        ( next, [], (Var.OpRef (SymbolIdentifier opSymbol), packageCondition left inners) : conditions )
      else
        ( left, (pre, op, post, next) : inners, conditions )

    _ ->
      ( left, (pre, op, post, next) : inners, conditions )


packageCondition :: Expr -> [(Comments, Var.Ref, Comments, Expr)] -> Expr
packageCondition left [] = left
packageCondition left inners =
    noRegion $ Binops left (reverse inners) False


done :: Bool -> State -> Expr'
done multiline (left, inners, []) = (Binops left (reverse inners) multiline)
done multiline (left, inners, conditions) =
  let
    finalConditions =
      reverse $ packageCondition left inners : conditions

    buildOp right =
      ([], Var.OpRef (SymbolIdentifier "&&"), [], right)
  in
    case finalConditions of
      first : r ->
        (Binops first (fmap buildOp r) multiline)


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Located a
noRegion =
    at nowhere nowhere
