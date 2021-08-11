{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Parse.Expression (term, typeAnnotation, definition, expr) where

import Data.Coapplicative
import qualified Data.Indexed as I
import Data.Maybe (fromMaybe)

import Parse.ParsecAdapter hiding (newline, spaces)
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
import AST.Structure
import ElmVersion
import Reporting.Annotation (Located)
import qualified Reporting.Annotation as A


--------  Basic Terms  --------

varTerm :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
varTerm elmVersion =
    fmap I.Fix $ addLocation $
    let
        resolve v =
            case v of
                TagRef [] (UppercaseIdentifier "True") -> Literal $ Boolean True
                TagRef [] (UppercaseIdentifier "False") -> Literal $ Boolean False
                _ -> VarExpr v
    in
        resolve <$> var elmVersion


accessor :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
accessor elmVersion =
  fmap I.Fix $ addLocation $
  do  lbl <- try (string "." >> rLabel elmVersion)
      return $ AccessFunction lbl


negative :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
negative elmVersion =
  fmap I.Fix $ addLocation $
  do  nTerm <-
          try $
            do  _ <- char '-'
                notFollowedBy (char '.' <|> char '-')
                term elmVersion

      return $ Unary Negative nTerm


--------  Complex Terms  --------

listTerm :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
listTerm elmVersion =
  fmap I.Fix $ addLocation $
    shader' <|> try (braces range) <|> commaSeparated
  where
    range =
      do
          lo <- expr elmVersion
          (C (loPost, hiPre) _) <- padded (string "..")
          hi <- expr elmVersion
          return $ \loPre hiPost multiline ->
              Range
                  (C (loPre, loPost) lo)
                  (C (hiPre, hiPost) hi)
                  multiline

    shader' =
      do  rawSrc <- Help.shader
          return $ GLShader (filter (/='\r') rawSrc)

    commaSeparated =
        braces' $ checkMultiline $
        do
            (terms, trailing) <- sectionedGroup (expr elmVersion)
            return $ ExplicitList terms trailing


parensTerm :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
parensTerm elmVersion =
  fmap I.Fix $
  choice
    [ try (addLocation $ parens' opFn )
    , try (addLocation $ parens' tupleFn)
    , do
          (start, e, end) <- located $ parens (parened <|> unit)
          return $ A.at start end e
    ]
  where
    opFn =
      VarExpr <$> anyOp elmVersion

    tupleFn =
      do  commas <- many1 comma
          return $ TupleFunction (length commas + 1)

    parened =
      do  expressions <- commaSep1 ((\e a b -> C (a, b) e) <$> expr elmVersion)
          return $ \pre post multiline ->
            case expressions pre post of
              [single] ->
                  Parens single

              expressions' ->
                  Tuple expressions' multiline

    unit =
        return $ \pre post _ -> Unit (pre ++ post)


recordTerm :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
recordTerm elmVersion =
    fmap I.Fix $
    addLocation $ brackets' $ checkMultiline $
        do
            base <- optionMaybe $ try (commented (lowVar elmVersion) <* string "|")
            (fields, trailing) <- sectionedGroup (pair (lowVar elmVersion) lenientEquals (expr elmVersion))
            return $ Record base fields trailing


term :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
term elmVersion =
  (choice
      [ fmap I.Fix $ addLocation (Literal <$> Literal.literal)
      , listTerm elmVersion
      , accessor elmVersion
      , negative elmVersion
      ]
  )
    <|> accessible elmVersion
        (varTerm elmVersion
            <|> parensTerm elmVersion
            <|> recordTerm elmVersion
        )
    <?> "an expression"


--------  Applications  --------

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a:_) = Just a


appExpr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
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
                    I.Fix $ A.at start end $ App t (fmap fst ts) multiline


--------  Normal Expressions  --------

expr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
expr elmVersion =
  choice [ letExpr elmVersion, caseExpr elmVersion, ifExpr elmVersion ]
    <|> lambdaExpr elmVersion
    <|> binaryExpr elmVersion
    <?> "an expression"


binaryExpr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
binaryExpr elmVersion =
    Binop.binops (appExpr elmVersion) lastExpr (anyOp elmVersion)
  where
    lastExpr =
        choice [ letExpr elmVersion, caseExpr elmVersion, ifExpr elmVersion ]
        <|> lambdaExpr elmVersion
        <?> "an expression"


ifExpr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
ifExpr elmVersion =
  let
    elseKeyword =
      (reserved elmVersion "else" <?> "an 'else' branch")
        >> whitespace
  in
  fmap I.Fix $ addLocation $
    do
      first <- ifClause elmVersion
      rest <- many (try $ C <$> elseKeyword <*> ifClause elmVersion)
      final <- C <$> elseKeyword <*> expr elmVersion

      return $ If first rest final


ifClause :: ElmVersion -> IParser (IfClause (ASTNS Located [UppercaseIdentifier] 'ExpressionNK))
ifClause elmVersion =
  do
    try (reserved elmVersion "if")
    preCondition <- whitespace
    condition <- expr elmVersion
    (C (postCondition, bodyComments) _) <- padded (reserved elmVersion "then")
    thenBranch <- expr elmVersion
    preElse <- whitespace <?> "an 'else' branch"
    return $ IfClause
      (C (preCondition, postCondition) condition)
      (C (bodyComments, preElse) thenBranch)


lambdaExpr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
lambdaExpr elmVersion =
  let
    subparser = do
      _ <- char '\\' <|> char '\x03BB' <?> "an anonymous function"
      args <- spacePrefix (Pattern.term elmVersion)
      (C (preArrowComments, bodyComments) _) <- padded rightArrow
      body <- expr elmVersion
      return (args, preArrowComments, bodyComments, body)
  in
    fmap I.Fix $ addLocation $
        do  ((args, preArrowComments, bodyComments, body), multiline) <- trackNewline subparser
            return $ Lambda args (preArrowComments ++ bodyComments) body $ multilineToBool multiline


caseExpr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
caseExpr elmVersion =
  fmap I.Fix $ addLocation $
  do  try (reserved elmVersion "case")
      (e, multilineSubject) <- trackNewline $ padded (expr elmVersion)
      reserved elmVersion "of"
      firstPatternComments <- whitespace
      result <- cases firstPatternComments
      return $ Case (e, multilineToBool multilineSubject) result
  where
    case_ preComments =
      fmap I.Fix $ addLocation $
      do
          (patternComments, p, C (preArrowComments, bodyComments) _) <-
              try ((,,)
                  <$> whitespace
                  <*> (checkIndent >> Pattern.expr elmVersion)
                  <*> padded rightArrow
                  )
          result <- expr elmVersion
          return $ CaseBranch
              { beforePattern = preComments ++ patternComments
              , beforeArrow = preArrowComments
              , afterArrow = bodyComments
              , pattern = p
              , body = result
              }

    cases preComments =
        withPos $
            do
                r1 <- case_ preComments
                r <- many $ case_ []
                return $ r1:r



-- LET


letExpr :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
letExpr elmVersion =
  fmap I.Fix $ addLocation $
  do  try (reserved elmVersion "let")
      A.A cal commentsAfterLet' <- addLocation whitespace
      let commentsAfterLet = fmap (I.Fix . A.A cal . LetComment) commentsAfterLet'
      defs <-
        block $
          do  def <- fmap I.Fix $ addLocation $ fmap (LetCommonDeclaration . I.Fix) $ addLocation (typeAnnotation elmVersion TypeAnnotation <|> definition elmVersion Definition)
              A.A cad commentsAfterDef' <- addLocation whitespace
              let commentsAfterDef = fmap (I.Fix . A.A cad . LetComment) commentsAfterDef'
              return (def : commentsAfterDef)
      _ <- reserved elmVersion "in"
      bodyComments <- whitespace
      Let (commentsAfterLet ++ concat defs) bodyComments <$> expr elmVersion



-- TYPE ANNOTATION

typeAnnotation :: ElmVersion -> (C1 after (Ref ()) -> C1 before (ASTNS Located [UppercaseIdentifier] 'TypeNK) -> a) -> IParser a
typeAnnotation elmVersion fn =
    (\(v, pre, post) e -> fn (C pre v) (C post e)) <$> try start <*> Type.expr elmVersion
  where
    start =
      do  v <- (VarRef () <$> lowVar elmVersion) <|> (OpRef <$> symOpInParens)
          (C (preColon, postColon) _) <- padded hasType
          return (v, preColon, postColon)


-- DEFINITION

definition ::
    ElmVersion
    ->
        (ASTNS Located [UppercaseIdentifier] 'PatternNK
          -> [C1 before (ASTNS Located [UppercaseIdentifier] 'PatternNK)]
          -> Comments
          -> (ASTNS Located [UppercaseIdentifier] 'ExpressionNK)
          -> a
        )
    -> IParser a
definition elmVersion fn =
  withPos $
    do
        (name, args) <- defStart elmVersion
        (C (preEqualsComments, postEqualsComments) _) <- padded equals
        body <- expr elmVersion
        return $ fn name args (preEqualsComments ++ postEqualsComments) body


defStart :: ElmVersion -> IParser (ASTNS Located [UppercaseIdentifier] 'PatternNK, [C1 before (ASTNS Located [UppercaseIdentifier] 'PatternNK)])
defStart elmVersion =
    choice
      [ do  pattern <- try $ Pattern.term elmVersion
            func $ pattern
      , do  opPattern <- fmap I.Fix $ addLocation (OpPattern <$> parens' symOp)
            func opPattern
      ]
      <?> "the definition of a variable (x = ...)"
  where
    func pattern =
        case extract $ I.unFix pattern of
          VarPattern _ ->
              ((,) pattern) <$> spacePrefix (Pattern.term elmVersion)

          OpPattern _ ->
              ((,) pattern) <$> spacePrefix (Pattern.term elmVersion)

          _ ->
              return (pattern, [])
