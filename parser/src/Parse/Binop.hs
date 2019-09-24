module Parse.Binop (binops) where

import Data.Fix
import Text.Parsec ((<|>), choice, try)

import qualified AST.Expression as E
import qualified AST.Variable as Var
import Parse.Helpers (commitIf, addLocation, multilineToBool)
import Parse.IParser
import Parse.Whitespace
import qualified Reporting.Annotation as A


binops
    :: IParser E.Expr
    -> IParser E.Expr
    -> IParser Var.Ref
    -> IParser E.Expr
binops term last anyOp =
  (fmap (Fix . E.AE) . addLocation) $
  do  ((e@(Fix (E.AE e')), ops), multiline) <- trackNewline ((,) <$> term <*> nextOps)
      return $
        case ops of
          [] ->
            A.drop e'
          _ ->
            E.Binops e ops $ multilineToBool multiline
  where
    nextOps =
      choice
        [ commitIf (whitespace >> anyOp) $
            do  preOpComments <- whitespace
                op <- anyOp
                preExpressionComments <- whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (E.BinopsClause preOpComments op preExpressionComments t) <$> nextOps
                  Right e -> return [E.BinopsClause preOpComments op preExpressionComments e]
        , return []
        ]
