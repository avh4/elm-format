module Parse.Expression (term, typeAnnotation, definition, expr) where

import Data.Maybe (fromMaybe)
import Text.Parsec hiding (newline, spaces)
import Text.Parsec.Indent (block, withPos, checkIndent)

import qualified Parse.Binop as Binop
import Parse.Helpers
import Parse.Common
import qualified Parse.Helpers as Help
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.State as State
import qualified Parse.Type as Type
import Parse.IParser
import Parse.Whitespace

import AST.V0_16
import qualified AST.Expression as E
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


--------  Basic Terms  --------

varTerm :: IParser E.Expr'
varTerm =
    let
        resolve v =
            case v of
                Var.TagRef [] (UppercaseIdentifier "True") -> E.Literal $ Boolean True
                Var.TagRef [] (UppercaseIdentifier "False") -> E.Literal $ Boolean False
                _ -> E.VarExpr v
    in
        resolve <$> var


accessor :: IParser E.Expr'
accessor =
  do  lbl <- try (string "." >> rLabel)
      return $ E.AccessFunction lbl


negative :: IParser E.Expr'
negative =
  do  nTerm <-
          try $
            do  _ <- char '-'
                notFollowedBy (char '.' <|> char '-')
                term

      return $ E.Unary E.Negative nTerm


--------  Complex Terms  --------

listTerm :: IParser E.Expr'
listTerm =
    shader' <|> try (braces range) <|> commaSeparated
  where
    range =
      do
          lo <- expr
          (loPost, _, hiPre) <- padded (string "..")
          hi <- expr
          return $ \loPre hiPost multiline ->
              E.Range
                  (Commented loPre lo loPost)
                  (Commented hiPre hi hiPost)
                  multiline

    shader' =
      do  rawSrc <- Help.shader
          return $ E.GLShader (filter (/='\r') rawSrc)

    commaSeparated =
        braces' $ checkMultiline $
        do
            (terms, trailing) <- sectionedGroup expr
            return $ E.ExplicitList terms trailing


parensTerm :: IParser E.Expr
parensTerm =
  choice
    [ try (addLocation $ parens' opFn)
    , try (addLocation $ parens' tupleFn)
    , do
          (start, e, end) <- located $ parens (parened <|> unit)
          return $ A.at start end e
    ]
  where
    opFn =
      E.VarExpr <$> anyOp

    tupleFn =
      do  commas <- many1 comma
          return $ E.TupleFunction (length commas + 1)

    parened =
      do  expressions <- commaSep1 ((\e a b -> Commented a e b) <$> expr)
          return $ \pre post multiline ->
            case expressions pre post of
              [single] ->
                  E.Parens single

              expressions' ->
                  E.Tuple expressions' multiline

    unit =
        return $ \pre post _ -> E.Unit (pre ++ post)


recordTerm :: IParser E.Expr
recordTerm =
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented lowVar <* string "|")
            (fields, trailing) <- sectionedGroup (pair lowVar lenientEquals expr)
            return $ E.Record base fields trailing


term :: IParser E.Expr
term =
  addLocation (choice [ E.Literal <$> Literal.literal, listTerm, accessor, negative ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "an expression"


--------  Applications  --------

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a:_) = Just a


appExpr :: IParser E.Expr
appExpr =
  expecting "an expression" $
  do  pushNewlineContext
      start <- getMyPosition
      t <- term
      sawNewlineInInitialTerm <- popNewlineContext
      ts <- constrainedSpacePrefix term
      end <- getMyPosition
      return $
          case ts of
            [] ->
              t
            _  ->
                let
                    multiline =
                        case
                            ( sawNewlineInInitialTerm
                            , fromMaybe (JoinAll) $ fmap snd $ head' ts
                            , any (isMultiline . snd) $ tail ts
                            )
                        of
                            (True, _, _ ) -> FASplitFirst
                            (False, JoinAll, True) -> FAJoinFirst SplitAll
                            (False, JoinAll, False) -> FAJoinFirst JoinAll
                            (False, SplitAll, _) -> FASplitFirst
                in
                    A.at start end $ E.App t (fmap fst ts) multiline


--------  Normal Expressions  --------

expr :: IParser E.Expr
expr =
  addLocation (choice [ letExpr, caseExpr, ifExpr ])
    <|> lambdaExpr
    <|> binaryExpr
    <?> "an expression"


binaryExpr :: IParser E.Expr
binaryExpr =
    Binop.binops appExpr lastExpr anyOp
  where
    lastExpr =
        addLocation (choice [ letExpr, caseExpr, ifExpr ])
        <|> lambdaExpr
        <?> "an expression"


ifExpr :: IParser E.Expr'
ifExpr =
  let
    elseKeyword =
      (reserved "else" <?> "an 'else' branch")
        >> whitespace
  in
    do
      first <- ifClause
      rest <- many (try $ (,) <$> elseKeyword <*> ifClause)
      final <- (,) <$> elseKeyword <*> expr

      return $ E.If first rest final


ifClause :: IParser E.IfClause
ifClause =
  do
    try (reserved "if")
    preCondition <- whitespace
    condition <- expr
    (postCondition, _, bodyComments) <- padded (reserved "then")
    thenBranch <- expr
    preElse <- whitespace <?> "an 'else' branch"
    return
      ( Commented preCondition condition postCondition
      , Commented bodyComments thenBranch preElse
      )


lambdaExpr :: IParser E.Expr
lambdaExpr =
  addLocation $
  do  pushNewlineContext
      _ <- char '\\' <|> char '\x03BB' <?> "an anonymous function"
      args <- spacePrefix Pattern.term
      (preArrowComments, _, bodyComments) <- padded rightArrow
      body <- expr
      multiline <- popNewlineContext
      return $ E.Lambda args (preArrowComments ++ bodyComments) body multiline


caseExpr :: IParser E.Expr'
caseExpr =
  do  try (reserved "case")
      pushNewlineContext
      e <- (\(pre, e, post) -> Commented pre e post) <$> padded expr
      reserved "of"
      multilineSubject <- popNewlineContext
      firstPatternComments <- whitespace
      updateState $ State.setNewline -- because if statements are always formatted as multiline, we pretend we saw a newline here to avoid problems with the Box rendering model
      result <- cases firstPatternComments
      return $ E.Case (e, multilineSubject) result
  where
    case_ preComments =
      do
          (patternComments, p, (preArrowComments, _, bodyComments)) <-
              try ((,,)
                  <$> whitespace
                  <*> (checkIndent >> Pattern.expr)
                  <*> padded rightArrow
                  )
          result <- expr
          return
            ( Commented (preComments ++ patternComments) p preArrowComments
            , (bodyComments, result)
            )

    cases preComments =
        withPos $
            do
                r1 <- case_ preComments
                r <- many $ case_ []
                return $ r1:r



-- LET


letExpr :: IParser E.Expr'
letExpr =
  do  try (reserved "let")
      commentsAfterLet <- map E.LetComment <$> whitespace
      defs <-
        block $
          do  def <- typeAnnotation E.LetAnnotation <|> definition E.LetDefinition
              commentsAfterDef <- whitespace
              return $ def : (map E.LetComment commentsAfterDef)
      _ <- reserved "in"
      bodyComments <- whitespace
      E.Let (commentsAfterLet ++ concat defs) bodyComments <$> expr



-- TYPE ANNOTATION

typeAnnotation :: ((Var.Ref, Comments) -> (Comments, Type) -> a) -> IParser a
typeAnnotation fn =
    (\(v, pre, post) e -> fn (v, pre) (post, e)) <$> try start <*> Type.expr
  where
    start =
      do  v <- (Var.VarRef [] <$> lowVar) <|> parens' (Var.OpRef <$> symOp)
          (preColon, _, postColon) <- padded hasType
          return (v, preColon, postColon)


-- DEFINITION

definition :: (P.Pattern -> [(Comments, P.Pattern)] -> Comments -> E.Expr -> a) -> IParser a
definition fn =
  withPos $
    do
        (name, args) <- defStart
        (preEqualsComments, _, postEqualsComments) <- padded equals
        body <- expr
        return $ fn name args (preEqualsComments ++ postEqualsComments) body


defStart :: IParser (P.Pattern, [(Comments, P.Pattern)])
defStart =
    choice
      [ do  pattern <- try Pattern.term
            func pattern
      , do  opPattern <- addLocation (P.OpPattern <$> parens' symOp)
            func opPattern
      ]
      <?> "the definition of a variable (x = ...)"
  where
    func pattern =
        case pattern of
          A.A _ (P.VarPattern _) ->
              ((,) pattern) <$> spacePrefix Pattern.term

          A.A _ (P.OpPattern _) ->
              ((,) pattern) <$> spacePrefix Pattern.term

          _ ->
              return (pattern, [])
