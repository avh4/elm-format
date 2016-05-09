module Parse.Binop (binops) where

import Text.Parsec ((<|>), choice, try)

import qualified AST.Expression as E
import qualified AST.Variable as Var
import Parse.Helpers (IParser, commitIf, whitespace, popNewlineContext, pushNewlineContext, addLocation)
import qualified Reporting.Annotation as A


binops
    :: IParser E.Expr
    -> IParser E.Expr
    -> IParser Var.Ref
    -> IParser E.Expr
binops term last anyOp =
  addLocation $
  do  pushNewlineContext
      e <- term
      ops <- nextOps
      sawNewline <- popNewlineContext
      return $
        case ops of
          [] ->
            A.drop e
          _ ->
            E.Binops e ops sawNewline
  where
    nextOps =
      choice
        [ commitIf (whitespace >> anyOp) $
            do  preOpComments <- whitespace
                op <- anyOp
                preExpressionComments <- whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (preOpComments, op, preExpressionComments, t) <$> nextOps
                  Right e -> return [(preOpComments, op, preExpressionComments, e)]
        , return []
        ]
