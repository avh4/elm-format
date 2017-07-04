module ElmFormat.Render.GroupBinops (extractAnds) where

import AST.V0_16
import AST.Expression
import Reporting.Annotation
import qualified AST.Variable as Var
import qualified Reporting.Region as Region


-- TODO: handle Ors also
-- TODO: ensure it only groups comparison operators?
extractAnds :: Expr -> Expr
extractAnds expr =
    case expr of
        A loc (Binops left ops multiline) ->
            A loc $ done multiline $ foldl step (left, [], []) ops

        _ ->
            expr


type State = (Expr, [(Comments, Var.Ref, Comments, Expr)], [Expr])


step :: State -> (Comments, Var.Ref, Comments, Expr) -> State
step (left, inners, conditions) (pre, op, post, next) =  -- TODO: Handle comments
  case op of
    Var.OpRef (SymbolIdentifier maybeAnd) ->
      if maybeAnd == "&&" then
        ( next, [], (noRegion $ Binops left inners False) : conditions ) -- TODO: Handle multiline
      else
        ( left, (pre, op, post, next) : inners, conditions )

    _ ->
      ( left, (pre, op, post, next) : inners, conditions )



done :: Bool -> State -> Expr'
done multiline (left, inners, []) = (Binops left (reverse inners) multiline)
done multiline (left, inners, conditions) =
  let
    finalConditions =
      reverse $ (noRegion $ Binops left (reverse inners) False) : conditions --TODO: use multiline -- TODO: reverse inners is not tested

    buildOp right =
      ([], Var.OpRef (SymbolIdentifier "&&"), [], right)
  in
    case finalConditions of
      first : r ->
        -- first
        (Binops first (fmap buildOp r) False) -- TODO: Handle multiline


nowhere :: Region.Position
nowhere =
    Region.Position 0 0


noRegion :: a -> Located a
noRegion =
    at nowhere nowhere
