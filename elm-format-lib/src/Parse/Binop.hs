{-# LANGUAGE DataKinds #-}
module Parse.Binop (binops) where

import Parse.ParsecAdapter ((<|>), choice, try)

import AST.V0_16
import Data.Coapplicative
import qualified Data.Indexed as I
import Parse.Helpers (commitIf, addLocation, multilineToBool)
import Parse.IParser
import Parse.Whitespace
import Reporting.Annotation (Located)


binops
    :: IParser (I.Fix2 Located (AST typeRef ctorRef varRef) 'ExpressionNK)
    -> IParser (I.Fix2 Located (AST typeRef ctorRef varRef) 'ExpressionNK)
    -> IParser varRef
    -> IParser (I.Fix2 Located (AST typeRef ctorRef varRef) 'ExpressionNK)
binops term last anyOp =
  fmap I.Fix2 $ addLocation $
  do  ((e, ops), multiline) <- trackNewline ((,) <$> term <*> nextOps)
      return $
        case ops of
          [] ->
            extract $ I.unFix2 e
          _ ->
            Binops e ops $ multilineToBool multiline
  where
    nextOps =
      choice
        [ commitIf (whitespace >> anyOp) $
            do  preOpComments <- whitespace
                op <- anyOp
                preExpressionComments <- whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (BinopsClause preOpComments op preExpressionComments t) <$> nextOps
                  Right e -> return [BinopsClause preOpComments op preExpressionComments e]
        , return []
        ]
