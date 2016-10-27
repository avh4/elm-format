{-# LANGUAGE FlexibleContexts #-}
module Parse.Helpers where

import Prelude hiding (until)
import Control.Monad (guard)
import Control.Monad.State (State)
import qualified Data.Char as Char
import qualified Data.List as List
import Text.Parsec hiding (newline, spaces, State)
import Text.Parsec.Indent (indented, runIndent)
import qualified Text.Parsec.Token as T

import AST.V0_16
import qualified AST.Expression
import qualified AST.Helpers as Help
import qualified AST.Variable
import qualified Parse.State as State
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax
import qualified Reporting.Region as R


reserveds :: [String]
reserveds =
    [ "if", "then", "else"
    , "case", "of"
    , "let", "in"
    , "type"
    , "module", "where"
    , "import", "as", "hiding", "exposing"
    , "port", "export", "foreign"
    , "deriving"
    ]


-- ERROR HELP

expecting = flip (<?>)


-- SETUP

type SourceM = State SourcePos
type IParser a = ParsecT String State.State SourceM a


iParse :: IParser a -> String -> Either ParseError a
iParse parser source =
  iParseWithState "" State.init parser source


iParseWithState :: SourceName -> State.State -> IParser a -> String -> Either ParseError a
iParseWithState sourceName state aParser input =
  runIndent sourceName $ runParserT aParser state sourceName input


-- VARIABLES

var :: IParser String
var =
  makeVar (letter <|> char '_') <?> "a name"


lowVar :: IParser String
lowVar =
  makeVar lower <?> "a lower case name"


capVar :: IParser String
capVar =
  makeVar upper <?> "an upper case name"


qualifiedVar :: IParser AST.Variable.Ref
qualifiedVar =
  do  vars <- many ((++) <$> capVar <*> string ".")
      var <- (++) (concat vars) <$> lowVar
      return $ AST.Variable.VarRef var


rLabel :: IParser String
rLabel = lowVar


innerVarChar :: IParser Char
innerVarChar =
  alphaNum <|> char '_' <|> char '\'' <?> "more letters in this name"


makeVar :: IParser Char -> IParser String
makeVar firstChar =
  do  variable <- (:) <$> firstChar <*> many innerVarChar
      if variable `elem` reserveds
        then fail (Syntax.keyword variable)
        else return variable


reserved :: String -> IParser ()
reserved word =
  expecting ("reserved word `" ++ word ++ "`") $
    do  string word
        notFollowedBy innerVarChar
        return ()


-- INFIX OPERATORS

anyOp :: IParser AST.Variable.Ref
anyOp =
  (betwixt '`' '`' qualifiedVar <?> "an infix operator like `andThen`")
  <|> symOp


symOp :: IParser AST.Variable.Ref
symOp =
  do  op <- many1 (satisfy Help.isSymbol) <?> "an infix operator like +"
      guard (op `notElem` [ "=", "..", "->", "--", "|", "\8594", ":" ])
      case op of
        "." -> notFollowedBy lower >> (return $ AST.Variable.OpRef op)
        _   -> return $ AST.Variable.OpRef op


-- COMMON SYMBOLS

equals :: IParser ()
equals =
  const () <$> char '=' <?> "="


lenientEquals :: IParser ()
lenientEquals =
  const () <$> (char '=' <|> char ':') <?> "="


rightArrow :: IParser ()
rightArrow =
  const () <$> (string "->" <|> string "\8594") <?> "->"


cons :: IParser ()
cons =
  const () <$> string "::" <?> "a cons operator '::'"


hasType :: IParser ()
hasType =
  const () <$> char ':' <?> "the \"has type\" symbol ':'"


lenientHasType :: IParser ()
lenientHasType =
  const () <$> (char ':' <|> char '=') <?> "the \"has type\" symbol ':'"


comma :: IParser ()
comma =
  const () <$> char ',' <?> "a comma ','"


semicolon :: IParser ()
semicolon =
  const () <$> char ';' <?> "a semicolon ';'"


verticalBar :: IParser ()
verticalBar =
  const () <$> char '|' <?> "a vertical bar '|'"


commitIf check p =
    commit <|> try p
  where
    commit =
      try (lookAhead check) >> p


-- SEPARATORS

parseWhile1 :: IParser (b, z, a) -> IParser (a -> b -> x) -> IParser (a -> b -> [x])
parseWhile1 sep parser =
    let
        -- step :: ((b, _, a), xff) -> ([x], (b -> x), b) -> ([x], (b -> x), b)
        step ((sepPre, _, sepPost), nextXf) (acc, xf, lastPost) =
            ((xf sepPre):acc, nextXf sepPost, lastPost)

        -- done :: ([x], xf, b) -> [x]
        done (acc, next, post) =
            List.reverse $ (next post):acc
    in
        do
            value <- parser
            zips <- many ((,) <$> try sep <*> parser)
            return $ \pre post -> done $ List.foldr step ([], value pre, post) (List.reverse zips)


spaceySepBy1 :: IParser sep -> IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
spaceySepBy1 sep parser =
    parseWhile1 (padded sep) parser


spaceySepBy1' :: IParser sep -> IParser a -> IParser (Comments -> Comments -> [Commented a])
spaceySepBy1' sep parser =
    parseWhile1 (padded sep) ((\x pre post -> Commented pre x post) <$> parser)


commaSep1 :: IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
commaSep1 =
  spaceySepBy1 comma


commaSep1' :: IParser a -> IParser (Comments -> Comments -> [Commented a])
commaSep1' =
  spaceySepBy1' comma


commaSep :: IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
commaSep =
  option (\_ _ -> []) . commaSep1 -- TODO: use comments for empty list


semiSep1 :: IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
semiSep1 =
  spaceySepBy1 semicolon


pipeSep1 :: IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
pipeSep1 =
  spaceySepBy1 verticalBar


keyValue :: IParser sep -> IParser key -> IParser val -> IParser (Comments -> Comments -> (Commented key, Commented val) )
keyValue parseSep parseKey parseVal =
  do
    key <- parseKey
    preSep <- whitespace <* parseSep
    postSep <- whitespace
    val <- parseVal
    return $ \pre post ->
      ( Commented pre key preSep
      , Commented postSep val post
      )


separated :: IParser sep -> IParser e -> IParser (Either e (R.Region, (e,Comments), [Commented e], (Comments,e), Bool))
separated sep expr' =
  do  start <- getMyPosition
      _ <- pushNewlineContext
      t1 <- expr'
      arrow <- optionMaybe $ try (whitespace <* sep)
      case arrow of
        Nothing ->
            do  _ <- popNewlineContext
                return $ Left t1
        Just preArrow ->
            do  postArrow <- whitespace
                t2 <- separated sep expr'
                end <- getMyPosition
                multiline <- popNewlineContext
                return $ Right $
                  case t2 of
                    Right (_, (t2',postT2), ts, tlast, _) ->
                      ( R.Region start end
                      , (t1, preArrow)
                      , ((Commented postArrow t2' postT2):ts)
                      , tlast
                      , multiline
                      )
                    Left t2' ->
                      ( R.Region start end
                      , (t1, preArrow)
                      , []
                      , (postArrow, t2')
                      , multiline)


dotSep1 :: IParser a -> IParser [a]
dotSep1 p =
  (:) <$> p <*> many (try (char '.') >> p)


spacePrefix :: IParser a -> IParser [(Comments, a)]
spacePrefix p =
  fmap fst <$>
      constrainedSpacePrefix' p (\_ -> return ())


constrainedSpacePrefix :: IParser a -> IParser [((Comments, a), Multiline)]
constrainedSpacePrefix parser =
  constrainedSpacePrefix' parser constraint
  where
    constraint empty = if empty then notFollowedBy (char '-') else return ()


constrainedSpacePrefix' :: IParser a -> (Bool -> IParser b) -> IParser [((Comments, a), Multiline)]
constrainedSpacePrefix' parser constraint =
    many $ trackNewline $ choice
      [ comment <$> try (const <$> spacing <*> lookAhead (oneOf "[({")) <*> parser
      , try (comment <$> spacing <*> parser)
      ]
    where
      comment pre value = (pre, value)

      spacing = do
        (n, comments) <- whitespace'
        _ <- constraint (not n) <?> Syntax.whitespace
        indented
        return comments


-- SURROUNDED BY

followedBy a b =
  do  x <- a
      b
      return x


betwixt :: Char -> Char -> IParser a -> IParser a
betwixt a b c =
  do  char a
      out <- c
      char b <?> "a closing '" ++ [b] ++ "'"
      return out


surround :: Char -> Char -> String -> IParser (Comments -> Comments -> Bool -> a) -> IParser a
surround a z name p = do
  pushNewlineContext
  char a
  (pre, v, post) <- padded p
  char z <?> unwords ["a closing", name, show z]
  multiline <- popNewlineContext
  return $ v pre post multiline


braces :: IParser (Comments -> Comments -> Bool -> a) -> IParser a
braces =
  surround '[' ']' "brace"


parens :: IParser (Comments -> Comments -> Bool -> a) -> IParser a
parens =
  surround '(' ')' "paren"


brackets :: IParser (Comments -> Comments -> Bool -> a) -> IParser a
brackets =
  surround '{' '}' "bracket"


surround' :: Char -> Char -> String -> IParser a -> IParser a
surround' a z name p = do
  char a
  v <- p
  char z <?> unwords ["a closing", name, show z]
  return v


parens' :: IParser a -> IParser a
parens' =
  surround' '(' ')' "paren"


parens'' :: IParser a -> IParser (Either Comments [Commented a])
parens'' = surround'' '(' ')'


braces'' :: IParser a -> IParser (Either Comments [Commented a])
braces'' = surround'' '[' ']'


surround'' :: Char -> Char -> IParser a -> IParser (Either Comments [Commented a])
surround'' leftDelim rightDelim inner =
  let
    sep''' =
      do
        v <- Commented <$> whitespace <*> inner <*> whitespace
        option [v] ((\x -> v : x) <$> (char ',' >> sep'''))
    sep'' =
      do
          pre <- whitespace
          v <- optionMaybe (Commented pre <$> inner <*> whitespace)
          case v of
              Nothing ->
                  return $ Left pre
              Just v' ->
                  Right <$> option [v'] ((\x -> v' : x) <$> (char ',' >> sep'''))
  in
    do
      char leftDelim
      vs <- sep''
      char rightDelim
      return vs


-- HELPERS FOR EXPRESSIONS

getMyPosition :: IParser R.Position
getMyPosition =
  R.fromSourcePos <$> getPosition


addLocation :: IParser a -> IParser (A.Located a)
addLocation expr =
  do  (start, e, end) <- located expr
      return (A.at start end e)


located :: IParser a -> IParser (R.Position, a, R.Position)
located parser =
  do  start <- getMyPosition
      value <- parser
      end <- getMyPosition
      return (start, value, end)


accessible :: IParser AST.Expression.Expr -> IParser AST.Expression.Expr
accessible exprParser =
  do  start <- getMyPosition

      annotatedRootExpr@(A.A _ rootExpr) <- exprParser

      access <- optionMaybe (try dot <?> "a field access like .name")

      case access of
        Nothing ->
          return annotatedRootExpr

        Just _ ->
          accessible $
            do  v <- var
                end <- getMyPosition
                return . A.at start end $
                    case rootExpr of
                      AST.Expression.VarExpr (AST.Variable.VarRef name@(c:_))
                        | Char.isUpper c ->
                            AST.Expression.VarExpr $ AST.Variable.VarRef (name ++ '.' : v)
                      _ ->
                        AST.Expression.Access annotatedRootExpr v


dot :: IParser ()
dot =
  do  char '.'
      notFollowedBy (char '.')


-- WHITESPACE

padded :: IParser a -> IParser (Comments, a, Comments)
padded p =
  do  pre <- whitespace
      out <- p
      post <- whitespace
      return (pre, out, post)


spaces :: IParser Comments
spaces =
  let
      blank = string " " >> return []
      comment = ((\x -> [x]) <$> multiComment)
      space =
        blank
        <|> (const [CommentTrickOpener] <$> (try $ string "{--}"))
        <|> comment
        <?> Syntax.whitespace
  in
      concat <$> many1 space


forcedWS :: IParser Comments
forcedWS =
  choice
    [ (++) <$> spaces <*> (concat <$> many nl_space)
    , concat <$> many1 nl_space
    ]
  where
    nl_space =
      try ((++) <$> (concat <$> many1 newline) <*> option [] spaces)


-- Just eats whitespace until the next meaningful character.
dumbWhitespace :: IParser Comments
dumbWhitespace =
  concat <$> many (spaces <|> newline)


whitespace' :: IParser (Bool, Comments)
whitespace' =
  option (False, []) ((,) True <$> forcedWS)


whitespace :: IParser Comments
whitespace =
  snd <$> whitespace'


freshLine :: IParser Comments
freshLine =
      concat <$> (try ((++) <$> many1 newline <*> many space_nl) <|> try (many1 space_nl)) <?> Syntax.freshLine
  where
    space_nl = try $ (++) <$> spaces <*> (concat <$> many1 newline)


newline :: IParser Comments
newline =
  do  result <- (simpleNewline >> return []) <|> ((\x -> [x]) <$> lineComment) <?> Syntax.newline
      updateState $ State.setNewline
      return result


simpleNewline :: IParser ()
simpleNewline =
  do  try (string "\r\n") <|> string "\n"
      return ()


pushNewlineContext :: IParser ()
pushNewlineContext =
    updateState State.pushNewlineContext


popNewlineContext :: IParser Bool
popNewlineContext =
  do  state <- getState
      updateState State.popNewlineContext
      return $ State.sawNewline state


trackNewline :: IParser a -> IParser (a, Multiline)
trackNewline parser =
    do
        updateState State.pushNewlineContext
        a <- parser
        state <- getState
        updateState State.popNewlineContext
        return (a, if State.sawNewline state then SplitAll else JoinAll)


lineComment :: IParser Comment
lineComment =
  do  try (string "--")
      choice
        [ const CommentTrickCloser
            <$> try (char '}' >> many (char ' ') >> (simpleNewline <|> eof))
        , do
            (comment, ()) <-
              anyUntil $ simpleNewline <|> eof
            return $ LineComment comment
        ]


commentedKeyword :: String -> IParser a -> IParser (KeywordCommented a)
commentedKeyword word parser =
  do
    pre <- try (whitespace <* reserved word)
    post <- whitespace
    value <- parser
    return $ KeywordCommented pre post value


docComment :: IParser String
docComment =
  do  try (string "{-|")
      many (string " ")
      contents <- closeComment False
      return contents


multiComment :: IParser Comment
multiComment =
  do  try (string "{-" <* notFollowedBy (string "|") )
      isCommentTrick <-
        choice
          [ char '-' >> return True
          , return False
          ]
      many (string " ")
      b <- closeComment False
      return $
        if isCommentTrick then
          CommentTrickBlock b
        else
          BlockComment $ trimIndent $ lines b
  where
      trimIndent [] = []
      trimIndent (l1:ls) =
          let
              depth = minimum $ map fst $ filter (\(a,b) -> a /= b) $ map (\l -> (length $ takeWhile Char.isSpace $ l, length l)) $ ls
          in
              l1 : (map (drop depth) ls)


closeComment :: Bool -> IParser String
closeComment keepClosingPunc =
  (\(a,b) -> a ++ b) <$>
    (anyUntil $
      choice $
        [ try ((\a b -> if keepClosingPunc then concat (a ++ [b]) else "") <$> many (string " ") <*> string "-}") <?> "the end of a comment -}"
        , concat <$> sequence [ try (string "{-"), closeComment True, closeComment keepClosingPunc]
        ])


-- ODD COMBINATORS

failure msg = do
  inp <- getInput
  setInput ('x':inp)
  anyToken
  fail msg


until :: IParser a -> IParser b -> IParser b
until p end =
    go
  where
    go = end <|> (p >> go)


anyUntil :: IParser a -> IParser (String, a)
anyUntil end =
    go ""
  where
    next pre =
      do
        nextChar <- anyChar
        go (nextChar : pre)

    go pre =
      ((,) (reverse pre) <$> end) <|> next pre


-- BASIC LANGUAGE LITERALS

shader :: IParser String
shader =
  do  try (string "[glsl|")
      rawSrc <- closeShader id
      return rawSrc


closeShader :: (String -> a) -> IParser a
closeShader builder =
  choice
    [ do  try (string "|]")
          return (builder "")
    , do  c <- anyChar
          closeShader (builder . (c:))
    ]


str :: IParser (String, Bool)
str =
  expecting "a string" $
  do  (s, multi) <- choice [ multiStr, singleStr ]
      result <- processAs T.stringLiteral . sandwich '\"' $ concat s
      return (result, multi)
  where
    rawString quote insides =
        quote >> manyTill insides quote

    multiStr  =
        do  result <- rawString (try (string "\"\"\"")) multilineStringChar
            return (result, True)
    singleStr =
        do  result <- rawString (char '"') stringChar
            return (result, False)

    stringChar :: IParser String
    stringChar = choice [ newlineChar, escaped '\"', (:[]) <$> satisfy (/= '\"') ]

    multilineStringChar :: IParser String
    multilineStringChar =
        do noEnd
           choice [ newlineChar, escaped '\"', expandQuote <$> anyChar ]
        where
          noEnd = notFollowedBy (string "\"\"\"")
          expandQuote c = if c == '\"' then "\\\"" else [c]

    newlineChar :: IParser String
    newlineChar =
        choice [ char '\n' >> return "\\n"
               , char '\r' >> return "\\r" ]


sandwich :: Char -> String -> String
sandwich delim s =
  delim : s ++ [delim]


escaped :: Char -> IParser String
escaped delim =
  try $ do
    char '\\'
    c <- char '\\' <|> char delim
    return ['\\', c]


chr :: IParser Char
chr =
    betwixt '\'' '\'' character <?> "a character"
  where
    nonQuote = satisfy (/='\'')

    character =
      do  c <- choice
                [ escaped '\''
                , (:) <$> char '\\' <*> many1 nonQuote
                , (:[]) <$> nonQuote
                ]

          processAs T.charLiteral $ sandwich '\'' c


processAs :: (T.GenTokenParser String u SourceM -> IParser a) -> String -> IParser a
processAs processor s =
    calloutParser s (processor lexer)
  where
    calloutParser :: String -> IParser a -> IParser a
    calloutParser inp p =
      either (fail . show) return (iParse p inp)

    lexer :: T.GenTokenParser String u SourceM
    lexer = T.makeTokenParser elmDef

    -- I don't know how many of these are necessary for charLiteral/stringLiteral
    elmDef :: T.GenLanguageDef String u SourceM
    elmDef =
      T.LanguageDef
        { T.commentStart    = "{-"
        , T.commentEnd      = "-}"
        , T.commentLine     = "--"
        , T.nestedComments  = True
        , T.identStart      = undefined
        , T.identLetter     = undefined
        , T.opStart         = undefined
        , T.opLetter        = undefined
        , T.reservedNames   = reserveds
        , T.reservedOpNames = [":", "->", "|"]
        , T.caseSensitive   = True
        }
