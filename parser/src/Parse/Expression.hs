module Parse.Expression (term, typeAnnotation, definition, expr) where

import qualified Data.List as List
import Text.Parsec hiding (newline, spaces)
import Text.Parsec.Indent (block, withPos, checkIndent)

import qualified Parse.Binop as Binop
import Parse.Helpers
import qualified Parse.Helpers as Help
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.State as State
import qualified Parse.Type as Type

import AST.V0_15
import qualified AST.Expression as E
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


--------  Basic Terms  --------

varTerm :: IParser E.Expr'
varTerm =
  boolean <|> ((\x -> E.Var $ Var.VarRef x) <$> var)


boolean :: IParser E.Expr'
boolean =
  let t = const (L.Boolean True) <$> string "True"
      f = const (L.Boolean False) <$> string "False"
  in
    E.Literal <$> try (t <|> f)


accessor :: IParser E.Expr'
accessor =
  do  lbl <- try (string "." >> rLabel)
      return $ E.AccessFunction lbl


negative :: IParser E.Expr'
negative =
  do  nTerm <-
          try $
            do  char '-'
                notFollowedBy (char '.' <|> char '-')
                term

      return $ E.Unary E.Negative nTerm


--------  Complex Terms  --------

listTerm :: IParser E.Expr'
listTerm =
    shader' <|> braces (try range <|> commaSeparated)
  where
    range =
      do
          lo <- expr
          (loPost, _, hiPre) <- padded (string "..")
          hi <- expr
          return $ \loPre hiPost multiline ->
              E.Range
                  (Commented loPre loPost lo)
                  (Commented hiPre hiPost hi)
                  multiline

    shader' =
      do  pos <- getPosition
          let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
          (rawSrc, tipe) <- Help.shader
          return $ E.GLShader uid (filter (/='\r') rawSrc) tipe

    commaSeparated =
      do  term <- commaSep ((\e pre post -> Commented pre post e) <$> expr)
          return $ \pre post multiline -> E.ExplicitList (term pre post) multiline


parensTerm :: IParser E.Expr
parensTerm =
  choice
    [ try (parens $ fmap (const . const . const) opFn) -- TODO: use comments
    , do
          (start, e, end) <- located $ parens (tupleFn <|> parened <|> unit)
          return $ A.at start end e
    ]
  where
    opFn =
      do  (start, op, end) <- located anyOp
          return $
            A.at start end $
              E.Var op

    tupleFn =
      do  commas <- comma >> many (whitespace >> comma) -- TODO: use comments
          return $
              \_ _ _ -> E.TupleFunction (length commas + 2) -- TODO: use comments

    parened =
      do  expressions <- commaSep1 ((\e a b -> Commented a b e) <$> expr)
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
  addLocation $ brackets $ choice
    [ do  starter <- try (addLocation rLabel)
          (_, postStarter) <- whitespace
          choice
            [ update starter postStarter
            , literal starter postStarter
            ]
    , return $ \pre post _ -> E.EmptyRecord (pre ++ post)
    ]
  where
    update (A.A ann starter) postStarter =
      do  try (string "|")
          (_, postBar) <- whitespace
          fields <- commaSep1 field
          return $ \pre post multiline -> (E.RecordUpdate (Commented pre postStarter $ A.A ann (E.Var $ Var.VarRef starter)) (fields postBar post) multiline)

    literal (A.A _ starter) postStarter =
      do  pushNewlineContext
          try equals -- TODO: can the try break newline tracking?
          (_, preExpr) <- whitespace
          value <- expr
          multiline' <- popNewlineContext
          (_, postExpr) <- whitespace
          choice
            [ do  try comma
                  (_, preNext) <- whitespace
                  fields <- commaSep field
                  return $ \pre post multiline -> (E.Record ((pre, starter, postStarter, Commented preExpr postExpr value, multiline') : (fields preNext post)) multiline)
            , return $ \pre post multiline -> (E.Record [(pre, starter, postStarter, Commented preExpr (postExpr ++ post) value, multiline')] multiline)
            ]

    field =
      do  pushNewlineContext
          key <- rLabel
          (postKey, _, preExpr) <- padded equals
          value <- expr
          multiline <- popNewlineContext
          return $ \pre post -> (pre, key, postKey, Commented preExpr post value, multiline)


term :: IParser E.Expr
term =
  addLocation (choice [ E.Literal <$> Literal.literal, listTerm, accessor, negative ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "an expression"


--------  Applications  --------

appExpr :: IParser E.Expr
appExpr =
  expecting "an expression" $
  do  pushNewlineContext
      t <- term
      ts <- constrainedSpacePrefix term
      sawNewline <- popNewlineContext
      return $
          case ts of
            [] -> t
            _  ->
                A.sameAs
                    (List.foldl' (\f (_,t) -> A.merge f t ()) (A.sameAs t ()) ts) -- TODO: simplify this code to merge the region annotations
                    (E.App t ts sawNewline)


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
  ifHelp []


ifHelp :: [(E.Expr, Bool, [Comment], E.Expr)] -> IParser E.Expr'
ifHelp branches =
  do  try (reserved "if")
      pushNewlineContext
      whitespace -- TODO: use comments
      condition <- expr
      multilineCondition <- popNewlineContext
      (_, _, bodyComments) <- padded (reserved "then") -- TODO: use pre comments
      updateState $ State.setNewline -- because if statements are always formatted as multiline, we pretend we saw a newline here to avoid problems with the Box rendering model
      thenBranch <- expr
      whitespace <?> "an 'else' branch" -- TODO: use comments
      reserved "else" <?> "an 'else' branch"
      (_, trailingComments) <- whitespace -- TODO: use trailingComments in the newBranch case
      let newBranches = (condition, multilineCondition, bodyComments, thenBranch) : branches
      choice
        [ ifHelp newBranches
        , E.If (reverse newBranches) trailingComments <$> expr
        ]


lambdaExpr :: IParser E.Expr
lambdaExpr =
  addLocation $
  do  pushNewlineContext
      char '\\' <|> char '\x03BB' <?> "an anonymous function"
      args <- spacePrefix Pattern.term
      (preArrowComments, _, bodyComments) <- padded rightArrow
      body <- expr
      multiline <- popNewlineContext
      return $ E.Lambda args (preArrowComments ++ bodyComments) body multiline


caseExpr :: IParser E.Expr'
caseExpr =
  do  try (reserved "case")
      pushNewlineContext
      (_, e, _) <- padded expr -- TODO: use comments
      reserved "of"
      multilineSubject <- popNewlineContext
      (_, firstPatternComments) <- whitespace
      updateState $ State.setNewline -- because if statements are always formatted as multiline, we pretend we saw a newline here to avoid problems with the Box rendering model
      result <- without firstPatternComments -- <|> with
      return $ E.Case (e, multilineSubject) result
  where
    case_ preComments =
      do
          ((_, patternComments), p, (_, _, bodyComments)) <-
              try ((,,)
                  <$> whitespace
                  <*> (checkIndent >> Pattern.expr)
                  <*> padded rightArrow -- TODO: use pre arrow comments
                  )
          result <- expr
          return (preComments++patternComments, p, bodyComments, result)


    -- bracketed case statements are removed in 0.16
    -- with =
    --   brackets (fmap (const . const . const) $ semiSep1 (case_ <?> "cases { x -> ... }")) -- TODO: use comments

    without preComments =
        withPos $
            do
                r1 <- case_ preComments
                r <- many $ case_ []
                return $ r1:r


-- LET

letExpr :: IParser E.Expr'
letExpr =
  do  try (reserved "let")
      commentsAfterLet <- map (A.atDontCare . E.LetComment) <$> snd <$> whitespace
      defs <-
        block $
          do  def <- typeAnnotation <|> definition
              (_, commentsAfterDef) <- whitespace
              return $ def : (map (A.atDontCare . E.LetComment) commentsAfterDef)
      (_, _, bodyComments) <- padded (reserved "in") -- TODO: pre comments are always empty because any whitespace was consumed before padded?
      E.Let (commentsAfterLet ++ concat defs) bodyComments <$> expr


-- TYPE ANNOTATION

typeAnnotation :: IParser E.Def
typeAnnotation =
    addLocation (E.TypeAnnotation <$> try start <*> Type.expr)
  where
    start =
      do  v <- (Var.VarRef <$> lowVar) <|> parens' symOp
          padded hasType -- TODO: use comments
          return v


-- DEFINITION

definition :: IParser E.Def
definition =
  addLocation $
  withPos $
    do  pushNewlineContext
        (name, args) <- defStart
        (preEqualsComments, _, postEqualsComments) <- padded equals
        body <- expr
        sawNewline <- popNewlineContext
        return $ E.Definition name args (preEqualsComments ++ postEqualsComments) body sawNewline


defStart :: IParser (P.Pattern, [([Comment], P.Pattern)])
defStart =
    choice
      [ do  pattern <- try Pattern.term
            infics pattern <|> func pattern
      , do  opPattern <- addLocation (P.Var <$> parens' symOp)
            func opPattern
      ]
      <?> "the definition of a variable (x = ...)"
  where
    func pattern =
        case pattern of
          A.A _ (P.Var _) ->
              ((,) pattern) <$> spacePrefix Pattern.term

          _ ->
              return (pattern, [])

    infics p1 =
      do  (start, op, end) <- try (whitespace >> located anyOp) -- TODO: use comments
          p2 <- (whitespace >> Pattern.term) -- TODO: use comments
          return (A.at start end (P.Var op), [([], p1), ([], p2) ])
