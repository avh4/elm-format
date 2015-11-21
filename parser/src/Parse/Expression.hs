module Parse.Expression (term, typeAnnotation, definition, expr) where

import qualified Data.List as List
import Text.Parsec hiding (newline, spaces)
import Text.Parsec.Indent (block, withPos)

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
    E.Literal <$> (t <|> f)


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
      do  term <- commaSep (const . const <$> expr) -- TODO: use comments
          return $ \_ _ multiline -> E.ExplicitList (term [] []) multiline -- TODO: use comments


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
      do  commas <- comma >> many (whitespace >> comma)
          return $
              \_ _ _ -> E.TupleFunction (length commas + 2) -- TODO: use comments

    parened =
      do  expressions <- commaSep1 ((\e a b -> Commented a b e) <$> expr)
          return $
            case expressions [] [] of -- TODO: pass comments
              [Commented _ _ expression] ->
                  \_ _ multiline -> E.Parens expression multiline -- TODO: use comments
              _ ->
                  \pre post multiline -> E.Tuple (expressions pre post) multiline

    unit =
        return $ \_ _ _ -> E.Unit -- TODO: use comments?


recordTerm :: IParser E.Expr
recordTerm =
  addLocation $ brackets $ choice
    [ do  starter <- try (addLocation rLabel)
          whitespace
          choice
            [ update starter
            , literal starter
            ]
    , return $ \_ _ multiline -> E.Record [] multiline -- TODO: use comments
    ]
  where
    update (A.A ann starter) =
      do  try (string "|")
          whitespace
          fields <- commaSep1 field
          return $ \_ _ multiline -> (E.RecordUpdate (A.A ann (E.Var $ Var.VarRef starter)) (fields [] []) multiline) -- TODO: use comments -- TODO: pass comments

    literal (A.A _ starter) =
      do  pushNewlineContext
          try equals -- TODO: can the try break newline tracking?
          whitespace
          value <- expr
          multiline' <- popNewlineContext
          whitespace
          choice
            [ do  try comma
                  whitespace
                  fields <- commaSep field
                  return $ \_ _ multiline -> (E.Record ((starter, value, multiline') : (fields [] [])) multiline) -- TODO: use comments -- TODO: pass comments
            , return $ \_ _ multiline -> (E.Record [(starter, value, multiline')] multiline) -- TODO: use comments
            ]

    field =
      do  pushNewlineContext
          key <- rLabel
          padded equals
          value <- expr
          multiline <- popNewlineContext
          return $ \_ _ -> (key, value, multiline) -- TODO: use comments


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
                    (List.foldl' (\f (Commented _ _ t) -> A.merge f t ()) (A.sameAs t ()) ts)
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


ifHelp :: [(E.Expr, Bool, E.Expr)] -> IParser E.Expr'
ifHelp branches =
  do  try (reserved "if")
      pushNewlineContext
      whitespace
      condition <- expr
      multilineCondition <- popNewlineContext
      padded (reserved "then")
      updateState $ State.setNewline -- because if statements are always formatted as multiline, we pretend we saw a newline here to avoid problems with the Box rendering model
      thenBranch <- expr
      whitespace <?> "an 'else' branch"
      reserved "else" <?> "an 'else' branch"
      whitespace
      let newBranches = (condition, multilineCondition, thenBranch) : branches
      choice
        [ ifHelp newBranches
        , E.If (reverse newBranches) <$> expr
        ]


lambdaExpr :: IParser E.Expr
lambdaExpr =
  addLocation $
  do  pushNewlineContext
      char '\\' <|> char '\x03BB' <?> "an anonymous function"
      whitespace
      args <- map (\(Commented _ _ v) -> v) <$> spaceSep1 Pattern.term -- TODO: use comments
      padded rightArrow
      body <- expr
      multiline <- popNewlineContext
      return $ E.Lambda args body multiline


caseExpr :: IParser E.Expr'
caseExpr =
  do  try (reserved "case")
      pushNewlineContext
      (_, e, _) <- padded expr -- TODO: use comments
      reserved "of"
      multilineSubject <- popNewlineContext
      whitespace
      updateState $ State.setNewline -- because if statements are always formatted as multiline, we pretend we saw a newline here to avoid problems with the Box rendering model
      result <- without -- <|> with
      return $ E.Case (e, multilineSubject) (result) -- TODO: pass comments
  where
    case_ =
      do  p <- Pattern.expr
          (_, _, bodyComments) <- padded rightArrow -- TODO: use pre comments
          result <- expr
          return $ \_ _ -> (p, bodyComments, result) -- TODO: use comments from bracketed cases

    -- bracketed case statements are removed in 0.16
    -- with =
    --   brackets (fmap (const . const . const) $ semiSep1 (case_ <?> "cases { x -> ... }")) -- TODO: use comments

    without =
        do
            result <- block (do c <- case_ ; whitespace ; return c)
            return $ fmap (\f -> f [] []) result -- TODO: use comments


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
          padded hasType
          return v


-- DEFINITION

definition :: IParser E.Def
definition =
  addLocation $
  withPos $
    do  pushNewlineContext
        (name:args) <- defStart
        (_, _, postEqualsComments) <- padded equals -- TODO: use pre comments
        body <- expr
        sawNewline <- popNewlineContext
        return $ E.Definition name args postEqualsComments body sawNewline


defStart :: IParser [P.Pattern]
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
              (pattern:) <$> map (\(Commented _ _ v) -> v) <$> spacePrefix Pattern.term -- TODO: use comments

          _ ->
              return [pattern]

    infics p1 =
      do  (start, op, end) <- try (whitespace >> located anyOp)
          p2 <- (whitespace >> Pattern.term)
          return [ A.at start end (P.Var op), p1, p2 ]
