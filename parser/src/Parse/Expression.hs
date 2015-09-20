module Parse.Expression (term, typeAnnotation, definition, expr) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.List as List
import Text.Parsec hiding (newline, spaces)
import Text.Parsec.Indent (block, withPos)

import qualified Parse.Binop as Binop
import Parse.Helpers
import qualified Parse.Helpers as Help
import qualified Parse.Literal as Literal
import qualified Parse.Pattern as Pattern
import qualified Parse.Type as Type

import qualified AST.Expression as E
import qualified AST.Literal as L
import qualified AST.Pattern as P
import qualified AST.Variable as Var
import qualified Reporting.Annotation as A


--------  Basic Terms  --------

varTerm :: IParser E.Expr'
varTerm =
  toVar <$> var


toVar :: String -> E.Expr'
toVar v =
  case v of
    "True" ->
        E.Literal (L.Boolean True)

    "False" ->
        E.Literal (L.Boolean False)

    _ ->
        E.rawVar v


accessor :: IParser E.Expr'
accessor =
  do  (start, lbl, end) <- located (try (string "." >> rLabel))

      let ann value =
            A.at start end value

      return $
        E.Lambda
            (ann (P.Var "_"))
            (ann (E.Access (ann (E.rawVar "_")) lbl))


negative :: IParser E.Expr'
negative =
  do  (start, nTerm, end) <-
          located $ try $
            do  char '-'
                notFollowedBy (char '.' <|> char '-')
                term

      let ann e =
            A.at start end e

      return $
        E.Binop
          (Var.OpRef "-")
          (ann (E.Literal (L.IntNum 0)))
          nTerm


--------  Complex Terms  --------

listTerm :: IParser E.Expr'
listTerm =
    shader' <|> braces (try range <|> E.ExplicitList <$> commaSep expr)
  where
    range =
      do  lo <- expr
          padded (string "..")
          E.Range lo <$> expr

    shader' =
      do  pos <- getPosition
          let uid = show (sourceLine pos) ++ ":" ++ show (sourceColumn pos)
          (rawSrc, tipe) <- Help.shader
          return $ E.GLShader uid (filter (/='\r') rawSrc) tipe


parensTerm :: IParser E.Expr
parensTerm =
  choice
    [ try (parens opFn)
    , parens (tupleFn <|> parened)
    ]
  where
    lambda start end x body =
        A.at start end (E.Lambda (A.at start end (P.Var x)) body)

    var start end x =
        A.at start end (E.rawVar x)

    opFn =
      do  (start, op, end) <- located anyOp
          return $
            A.at start end $
              E.Var (Var.OpRef op)

    tupleFn =
      do  (start, commas, end) <-
              located (comma >> many (whitespace >> comma))

          let vars = map (('v':) . show) [ 0 .. length commas + 1 ]

          return $
            foldr
              (lambda start end)
              (A.at start end (E.tuple (map (var start end) vars)))
              vars

    parened =
      do  (start, expressions, end) <- located (commaSep expr)
          return $
            case expressions of
              [expression] ->
                  expression
              _ ->
                  A.at start end (E.tuple expressions)


recordTerm :: IParser E.Expr
recordTerm =
  addLocation $ brackets $ choice $
    [ do  starter <- try (addLocation rLabel)
          whitespace
          choice
            [ update starter
            , literal starter
            ]
    , return (E.Record [])
    ]
  where
    update (A.A ann starter) =
      do  try (string "|")
          whitespace
          fields <- commaSep1 field
          return (E.Update (A.A ann (E.rawVar starter)) fields)

    literal (A.A _ starter) =
      do  try equals
          whitespace
          value <- expr
          whitespace
          choice
            [ do  try comma
                  whitespace
                  fields <- commaSep field
                  return (E.Record ((starter, value) : fields))
            , return (E.Record [(starter, value)])
            ]

    field =
      do  key <- rLabel
          padded equals
          value <- expr
          return (key, value)


term :: IParser E.Expr
term =
  addLocation (choice [ E.Literal <$> Literal.literal, listTerm, accessor, negative ])
    <|> accessible (addLocation varTerm <|> parensTerm <|> recordTerm)
    <?> "an expression"


--------  Applications  --------

appExpr :: IParser E.Expr
appExpr =
  expecting "an expression" $
  do  t <- term
      ts <- constrainedSpacePrefix term $ \str ->
                if null str then notFollowedBy (char '-') else return ()
      return $
          case ts of
            [] -> t
            _  -> List.foldl' (\f x -> A.merge f x $ E.App f x) t ts


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


ifHelp :: [(E.Expr, E.Expr)] -> IParser E.Expr'
ifHelp branches =
  do  try (reserved "if")
      whitespace
      condition <- expr
      padded (reserved "then")
      thenBranch <- expr
      whitespace <?> "an 'else' branch"
      reserved "else" <?> "an 'else' branch"
      whitespace
      let newBranches = (condition, thenBranch) : branches
      choice
        [ ifHelp newBranches
        , E.If (reverse newBranches) <$> expr
        ]


lambdaExpr :: IParser E.Expr
lambdaExpr =
  do  char '\\' <|> char '\x03BB' <?> "an anonymous function"
      whitespace
      args <- spaceSep1 Pattern.term
      padded rightArrow
      body <- expr
      return (makeFunction args body)


caseExpr :: IParser E.Expr'
caseExpr =
  do  try (reserved "case")
      e <- padded expr
      reserved "of"
      whitespace
      E.Case e <$> (with <|> without)
  where
    case_ =
      do  p <- Pattern.expr
          padded rightArrow
          (,) p <$> expr

    with =
      brackets (semiSep1 (case_ <?> "cases { x -> ... }"))

    without =
      block (do c <- case_ ; whitespace ; return c)


-- LET

letExpr :: IParser E.Expr'
letExpr =
  do  try (reserved "let")
      whitespace
      defs <-
        block $
          do  def <- typeAnnotation <|> definition
              whitespace
              return def
      padded (reserved "in")
      E.Let defs <$> expr


-- TYPE ANNOTATION

typeAnnotation :: IParser E.Def
typeAnnotation =
    addLocation (E.TypeAnnotation <$> try start <*> Type.expr)
  where
    start =
      do  v <- lowVar <|> parens symOp
          padded hasType
          return v


-- DEFINITION

definition :: IParser E.Def
definition =
  addLocation $
  withPos $
    do  (name:args) <- defStart
        padded equals
        body <- expr
        return . E.Definition name $ makeFunction args body


makeFunction :: [P.Pattern] -> E.Expr -> E.Expr
makeFunction args body@(A.A ann _) =
    foldr (\arg body' -> A.A ann $ E.Lambda arg body') body args


defStart :: IParser [P.Pattern]
defStart =
    choice
      [ do  pattern <- try Pattern.term
            infics pattern <|> func pattern
      , do  opPattern <- addLocation (P.Var <$> parens symOp)
            func opPattern
      ]
      <?> "the definition of a variable (x = ...)"
  where
    func pattern =
        case pattern of
          A.A _ (P.Var _) ->
              (pattern:) <$> spacePrefix Pattern.term

          _ ->
              return [pattern]

    infics p1 =
      do  (start, o:p, end) <- try (whitespace >> located anyOp)
          p2 <- (whitespace >> Pattern.term)
          let opName =
                if o == '`' then takeWhile (/='`') p else o:p
          return [ A.at start end (P.Var opName), p1, p2 ]
