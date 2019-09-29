module Parse.Binop (binops) where

import AST.V0_16
import Data.Fix
import Text.Parsec ((<|>), choice, try)

import AST.Expression (Expr, AnnotatedExpression(AE))
import qualified AST.Expression as E
import AST.Variable (Ref)
import Parse.Helpers (commitIf, addLocation, multilineToBool)
import Parse.IParser
import Parse.Whitespace
import qualified Reporting.Annotation as A


binops
    :: IParser Expr
    -> IParser Expr
    -> IParser (Ref [UppercaseIdentifier])
    -> IParser Expr
binops term last anyOp =
  (fmap (Fix . AE) . addLocation) $
  do  ((e@(Fix (AE e')), ops), multiline) <- trackNewline ((,) <$> term <*> nextOps)
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
