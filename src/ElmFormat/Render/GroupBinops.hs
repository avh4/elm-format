module ElmFormat.Render.GroupBinops (extractAnds) where

import AST.V0_16
import AST.Expression
import Reporting.Annotation
import qualified AST.Variable as Var

extractAnds :: Expr -> Expr
extractAnds expr =
    case expr of
        A loc (Binops left ops multiline) ->
            A loc $ done $ foldl step (left, [], []) ops

        _ ->
            expr


type State = (Expr, [(Comments, Var.Ref, Comments, Expr)], [Expr])


step :: State -> (Comments, Var.Ref, Comments, Expr) -> State
step (left, inners, outers) (pre, op, post, next) =
  (left, inners, outers)


done :: State -> Expr'
done (left, inners, outers) =
    Unit []
