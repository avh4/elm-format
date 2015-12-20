module Parse.Binop (binops) where

import qualified Data.List as List
import Text.Parsec ((<|>), choice, try)

import AST.V0_16
import qualified AST.Expression as E
import qualified AST.Variable as Var
import Parse.Helpers (IParser, commitIf, whitespace, popNewlineContext, pushNewlineContext, located)
import qualified Reporting.Annotation as A


binops
    :: IParser E.Expr
    -> IParser E.Expr
    -> IParser Var.Ref
    -> IParser E.Expr
binops term last anyOp =
  do  pushNewlineContext
      (start, e, _) <- located term
      (_, ops, end) <- located nextOps
      sawNewline <- popNewlineContext
      return $ A.at start end $ A.drop $ split e ops sawNewline -- TODO: the main purpose of split is to merge the annotations, which it does incorrectly and is overridden here, so it should probably go away
  where
    nextOps =
      choice
        [ commitIf (whitespace >> anyOp) $
            do  (_, preOpComments) <- whitespace
                op <- anyOp
                (_, preExpressionComments) <- whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (preOpComments, op, preExpressionComments, t) <$> nextOps
                  Right e -> return [(preOpComments, op, preExpressionComments, e)]
        , return []
        ]


split
    :: E.Expr
    -> [([Comment], Var.Ref, [Comment], E.Expr)]
    -> Bool
    -> E.Expr
split e0 [] _ = e0
split e0 ops multiline =
    let
        init :: A.Located (E.Expr,[([Comment], Var.Ref, [Comment], E.Expr)])
        init = A.sameAs e0 (e0,[])

        merge'
          :: ([Comment], Var.Ref, [Comment], E.Expr)
          -> A.Located x
          -> (E.Expr,[([Comment], Var.Ref, [Comment], E.Expr)])
          -> A.Located (E.Expr,[([Comment], Var.Ref, [Comment], E.Expr)])
        merge' (po,o,pe,e) loc (e0,ops) =
          A.merge e loc (e0,(po,o,pe,e):ops)

        merge
          :: ([Comment], Var.Ref, [Comment], E.Expr)
          -> A.Located (E.Expr,[([Comment], Var.Ref, [Comment], E.Expr)])
          -> A.Located (E.Expr,[([Comment], Var.Ref, [Comment], E.Expr)])
        merge x loc =
          merge' x loc (A.drop loc)

        wrap :: (E.Expr,[([Comment], Var.Ref, [Comment], E.Expr)]) -> E.Expr'
        wrap (e',ops) = E.Binops e' ops multiline
    in
      A.map wrap $ List.foldr merge init ops
