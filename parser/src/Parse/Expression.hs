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
import qualified Parse.Type as Type
import Parse.IParser
import Parse.Whitespace

import AST.V0_16
import qualified AST.Expression as E
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import ElmVersion
import qualified Reporting.Annotation as A


--------  Basic Terms  --------

varTerm :: ElmVersion -> IParser E.Expr'
varTerm elmVersion =
    let
        resolve v =
            case v of
                Var.TagRef [] (UppercaseIdentifier "True") -> E.Literal $ Boolean True
                Var.TagRef [] (UppercaseIdentifier "False") -> E.Literal $ Boolean False
                _ -> E.VarExpr v
    in
        resolve <$> var elmVersion


accessor :: ElmVersion -> IParser E.Expr'
accessor elmVersion =
  do  lbl <- try (string "." >> rLabel elmVersion)
      return $ E.AccessFunction lbl


negative :: ElmVersion -> IParser E.Expr'
negative elmVersion =
  do  nTerm <-
          try $
            do  _ <- char '-'
                notFollowedBy (char '.' <|> char '-')
                term elmVersion

      return $ E.Unary E.Negative nTerm


--------  Complex Terms  --------

listTerm :: ElmVersion -> IParser E.Expr'
listTerm elmVersion =
    shader' <|> try (braces range) <|> commaSeparated
  where
    range =
      do
          lo <- expr elmVersion
          (loPost, _, hiPre) <- padded (string "..")
          hi <- expr elmVersion
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
            (terms, trailing) <- sectionedGroup (expr elmVersion)
            return $ E.ExplicitList terms trailing


parensTerm :: ElmVersion -> IParser E.Expr
parensTerm elmVersion =
  choice
    [ try (addLocation $ parens' opFn )
    , try (addLocation $ parens' tupleFn)
    , do
          (start, e, end) <- located $ parens (parened <|> unit)
          return $ A.at start end e
    ]
  where
    opFn =
      E.VarExpr <$> anyOp elmVersion

    tupleFn =
      do  commas <- many1 comma
          return $ E.TupleFunction (length commas + 1)

    parened =
      do  expressions <- commaSep1 ((\e a b -> Commented a e b) <$> expr elmVersion)
          return $ \pre post multiline ->
            case expressions pre post of
              [single] ->
                  E.Parens single

              expressions' ->
                  E.Tuple expressions' multiline

    unit =
        return $ \pre post _ -> E.Unit (pre ++ post)


recordTerm :: ElmVersion -> IParser E.Expr
recordTerm elmVersion =
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented (lowVar elmVersion) <* string "|")
            (fields, trailing) <- sectionedGroup (pair (lowVar elmVersion) lenientEquals (expr elmVersion))
            return $ E.Record base fields trailing


term :: ElmVersion -> IParser E.Expr
term elmVersion =
  addLocation (choice [ E.Literal <$> Literal.literal, listTerm elmVersion, accessor elmVersion, negative elmVersion ])
    <|> accessible elmVersion (addLocation (varTerm elmVersion) <|> parensTerm elmVersion <|> recordTerm elmVersion)
    <?> "an expression"


--------  Applications  --------

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a:_) = Just a


appExpr :: ElmVersion -> IParser E.Expr
appExpr elmVersion =
  expecting "an expression" $
  do  start <- getMyPosition
      (t, initialTermMultiline) <- trackNewline (term elmVersion)
      ts <- constrainedSpacePrefix (term elmVersion)
      end <- getMyPosition
      return $
          case ts of
            [] ->
              t
            _  ->
                let
                    multiline =
                        case
                            ( initialTermMultiline
                            , fromMaybe (JoinAll) $ fmap snd $ head' ts
                            , any (isMultiline . snd) $ tail ts
                            )
                        of
                            (SplitAll, _, _ ) -> FASplitFirst
                            (JoinAll, JoinAll, True) -> FAJoinFirst SplitAll
                            (JoinAll, JoinAll, False) -> FAJoinFirst JoinAll
                            (JoinAll, SplitAll, _) -> FASplitFirst
                in
                    A.at start end $ E.App t (fmap fst ts) multiline


--------  Normal Expressions  --------

expr :: ElmVersion -> IParser E.Expr
expr elmVersion =
  addLocation (choice [ letExpr elmVersion, caseExpr elmVersion, ifExpr elmVersion ])
    <|> lambdaExpr elmVersion
    <|> binaryExpr elmVersion
    <?> "an expression"


binaryExpr :: ElmVersion -> IParser E.Expr
binaryExpr elmVersion =
    Binop.binops (appExpr elmVersion) lastExpr (anyOp elmVersion)
  where
    lastExpr =
        addLocation (choice [ letExpr elmVersion, caseExpr elmVersion, ifExpr elmVersion ])
        <|> lambdaExpr elmVersion
        <?> "an expression"


ifExpr :: ElmVersion -> IParser E.Expr'
ifExpr elmVersion =
  let
    elseKeyword =
      (reserved elmVersion "else" <?> "an 'else' branch")
        >> whitespace
  in
    do
      first <- ifClause elmVersion
      rest <- many (try $ (,) <$> elseKeyword <*> ifClause elmVersion)
      final <- (,) <$> elseKeyword <*> expr elmVersion

      return $ E.If first rest final


ifClause :: ElmVersion -> IParser E.IfClause
ifClause elmVersion =
  do
    try (reserved elmVersion "if")
    preCondition <- whitespace
    condition <- expr elmVersion
    (postCondition, _, bodyComments) <- padded (reserved elmVersion "then")
    thenBranch <- expr elmVersion
    preElse <- whitespace <?> "an 'else' branch"
    return
      ( Commented preCondition condition postCondition
      , Commented bodyComments thenBranch preElse
      )


lambdaExpr :: ElmVersion -> IParser E.Expr
lambdaExpr elmVersion =
  let
    subparser = do
      _ <- char '\\' <|> char '\x03BB' <?> "an anonymous function"
      args <- spacePrefix (Pattern.term elmVersion)
      (preArrowComments, _, bodyComments) <- padded rightArrow
      body <- expr elmVersion
      return (args, preArrowComments, bodyComments, body)
  in
    addLocation $
        do  ((args, preArrowComments, bodyComments, body), multiline) <- trackNewline subparser
            return $ E.Lambda args (preArrowComments ++ bodyComments) body $ multilineToBool multiline


caseExpr :: ElmVersion -> IParser E.Expr'
caseExpr elmVersion =
  do  try (reserved elmVersion "case")
      (e, multilineSubject) <- trackNewline $ (\(pre, e, post) -> Commented pre e post) <$> padded (expr elmVersion)
      reserved elmVersion "of"
      firstPatternComments <- whitespace
      result <- cases firstPatternComments
      return $ E.Case (e, multilineToBool multilineSubject) result
  where
    case_ preComments =
      do
          (patternComments, p, (preArrowComments, _, bodyComments)) <-
              try ((,,)
                  <$> whitespace
                  <*> (checkIndent >> Pattern.expr elmVersion)
                  <*> padded rightArrow
                  )
          result <- expr elmVersion
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


letExpr :: ElmVersion -> IParser E.Expr'
letExpr elmVersion =
  do  try (reserved elmVersion "let")
      commentsAfterLet <- map E.LetComment <$> whitespace
      defs <-
        block $
          do  def <- typeAnnotation elmVersion E.LetAnnotation <|> definition elmVersion E.LetDefinition
              commentsAfterDef <- whitespace
              return $ def : (map E.LetComment commentsAfterDef)
      _ <- reserved elmVersion "in"
      bodyComments <- whitespace
      E.Let (commentsAfterLet ++ concat defs) bodyComments <$> expr elmVersion



-- TYPE ANNOTATION

typeAnnotation :: ElmVersion -> ((Var.Ref, Comments) -> (Comments, Type) -> a) -> IParser a
typeAnnotation elmVersion fn =
    (\(v, pre, post) e -> fn (v, pre) (post, e)) <$> try start <*> Type.expr elmVersion
  where
    start =
      do  v <- (Var.VarRef [] <$> lowVar elmVersion) <|> (Var.OpRef <$> symOpInParens)
          (preColon, _, postColon) <- padded hasType
          return (v, preColon, postColon)


-- DEFINITION

definition :: ElmVersion -> (P.Pattern -> [(Comments, P.Pattern)] -> Comments -> E.Expr -> a) -> IParser a
definition elmVersion fn =
  withPos $
    do
        (name, args) <- defStart elmVersion
        (preEqualsComments, _, postEqualsComments) <- padded equals
        body <- expr elmVersion
        return $ fn name args (preEqualsComments ++ postEqualsComments) body


defStart :: ElmVersion -> IParser (P.Pattern, [(Comments, P.Pattern)])
defStart elmVersion =
    choice
      [ do  pattern <- try $ Pattern.term elmVersion
            func pattern
      , do  opPattern <- addLocation (P.OpPattern <$> parens' symOp)
            func opPattern
      ]
      <?> "the definition of a variable (x = ...)"
  where
    func pattern =
        case pattern of
          A.A _ (P.VarPattern _) ->
              ((,) pattern) <$> spacePrefix (Pattern.term elmVersion)

          A.A _ (P.OpPattern _) ->
              ((,) pattern) <$> spacePrefix (Pattern.term elmVersion)

          _ ->
              return (pattern, [])
