module Parse.Binop (binops) where

import qualified Data.List as List
import Text.Parsec ((<|>), choice, try)

import AST.V0_15
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
                let op' = Commented' preOpComments [] op
                (_, preExpressionComments) <- whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (op',Commented' preExpressionComments [] t) <$> nextOps
                  Right e -> return [(op', Commented' preExpressionComments [] e)]
        , return []
        ]


split
    :: E.Expr
    -> [(Commented' Var.Ref, Commented' E.Expr)]
    -> Bool
    -> E.Expr
split e0 [] _ = e0
split e0 ops multiline =
    let
        init :: A.Located (E.Expr,[(Commented' Var.Ref, Commented' E.Expr)])
        init = A.sameAs e0 (e0,[])

        merge'
          :: (Commented' Var.Ref, Commented' E.Expr)
          -> A.Located x
          -> (E.Expr,[(Commented' Var.Ref, Commented' E.Expr)])
          -> A.Located (E.Expr,[(Commented' Var.Ref, Commented' E.Expr)])
        merge' (o,e) loc (e0,ops) =
          A.merge ((\(Commented' _ _ v) -> v) $ e) loc (e0,(o,e):ops)

        merge
          :: (Commented' Var.Ref, Commented' E.Expr)
          -> A.Located (E.Expr,[(Commented' Var.Ref, Commented' E.Expr)])
          -> A.Located (E.Expr,[(Commented' Var.Ref, Commented' E.Expr)])
        merge (o,e) loc =
          merge' (o,e) loc (A.drop loc)

        wrap :: (E.Expr,[(Commented' Var.Ref, Commented' E.Expr)]) -> E.Expr'
        wrap (e',ops) = E.Binops e' ops multiline
    in
      A.map wrap $ List.foldr merge init ops
