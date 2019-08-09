{-# LANGUAGE FlexibleContexts #-}
module Parse.Helpers where

import Prelude hiding (until)
import Control.Monad (guard)
import Data.Map.Strict hiding (foldl)
import qualified Data.Maybe as Maybe
import Text.Parsec hiding (newline, spaces, State)
import Text.Parsec.Indent (indented, runIndent)

import AST.V0_16
import qualified AST.Expression
import qualified AST.Helpers as Help
import qualified AST.Variable
import ElmVersion
import qualified Parse.State as State
import Parse.Comments
import Parse.IParser
import Parse.Whitespace
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
    , "import", "exposing"
    , "as"
    , "port"
    ]

-- ERROR HELP

expecting :: String -> IParser a -> IParser a
expecting = flip (<?>)


-- SETUP

iParse :: IParser a -> String -> Either ParseError a
iParse =
    iParseWithState "" State.init


iParseWithState :: SourceName -> State.State -> IParser a -> String -> Either ParseError a
iParseWithState sourceName state aParser input =
  runIndent sourceName $ runParserT aParser state sourceName input


-- VARIABLES

var :: ElmVersion -> IParser AST.Variable.Ref
var elmVersion =
  try (qualifiedVar elmVersion) <|> qualifiedTag elmVersion <?> "a name"


lowVar :: ElmVersion -> IParser LowercaseIdentifier
lowVar elmVersion =
  LowercaseIdentifier <$> makeVar elmVersion lower <?> "a lower case name"


capVar :: ElmVersion -> IParser UppercaseIdentifier
capVar elmVersion =
  UppercaseIdentifier <$> makeVar elmVersion upper <?> "an upper case name"


qualifiedVar :: ElmVersion -> IParser AST.Variable.Ref
qualifiedVar elmVersion =
    AST.Variable.VarRef
        <$> many (const <$> capVar elmVersion <*> string ".")
        <*> lowVar elmVersion


qualifiedTag :: ElmVersion -> IParser AST.Variable.Ref
qualifiedTag elmVersion =
    AST.Variable.TagRef
        <$> many (try $ const <$> capVar elmVersion <*> string ".")
        <*> capVar elmVersion


rLabel :: ElmVersion -> IParser LowercaseIdentifier
rLabel = lowVar


innerVarChar :: ElmVersion -> IParser Char
innerVarChar elmVersion =
    if syntax_0_19_disallowApostropheInVars elmVersion
        then alphaNum <|> char '_' <?> "more letters in this name"
        else alphaNum <|> char '_' <|> char '\'' <?> "more letters in this name"


makeVar :: ElmVersion -> IParser Char -> IParser String
makeVar elmVersion firstChar =
  do  variable <- (:) <$> firstChar <*> many (innerVarChar elmVersion)
      if variable `elem` reserveds
        then fail (Syntax.keyword variable)
        else return variable


reserved :: ElmVersion -> String -> IParser ()
reserved elmVersion word =
  expecting ("reserved word `" ++ word ++ "`") $
    do  _ <- string word
        notFollowedBy (innerVarChar elmVersion)
        return ()


-- INFIX OPERATORS

anyOp :: ElmVersion -> IParser AST.Variable.Ref
anyOp elmVersion =
  (betwixt '`' '`' (qualifiedVar elmVersion) <?> "an infix operator like `andThen`")
  <|> (AST.Variable.OpRef <$> symOp)


symOp :: IParser SymbolIdentifier
symOp =
  do  op <- many1 (satisfy Help.isSymbol) <?> "an infix operator like +"
      guard (op `notElem` [ "=", "..", "->", "--", "|", "\8594", ":" ])
      case op of
        "." -> notFollowedBy lower >> return (SymbolIdentifier op)
        _   -> return $ SymbolIdentifier op


symOpInParens :: IParser SymbolIdentifier
symOpInParens =
    parens' symOp


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


commitIf :: IParser any -> IParser a -> IParser a
commitIf check p =
    commit <|> try p
  where
    commit =
      try (lookAhead check) >> p


-- SEPARATORS


spaceySepBy1 :: IParser sep -> IParser a -> IParser (ExposedCommentedList a)
spaceySepBy1 sep parser =
    let
        -- step :: PostCommented (WithEol a) -> [Commented (WithEol a)] -> Comments -> IParser (ExposedCommentedList a)
        step first rest post =
            do
                next <- withEol parser
                choice
                    [ try (padded sep)
                        >>= (\(preSep, _, postSep) -> step first (Commented post next preSep : rest) postSep)
                    , return $ Multiple first (reverse rest) (post, next)
                    ]

    in
        do
            value <- withEol parser
            choice
                [ try (padded sep)
                    >>= (\(preSep, _, postSep) -> step (value, preSep) [] postSep)
                , return $ Single value
                ]


-- DEPRECATED: use spaceySepBy1 instead
spaceySepBy1'' :: IParser sep -> IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
spaceySepBy1'' sep parser =
  let
    combine eol post =
      Maybe.maybeToList (fmap LineComment eol) ++ post
  in
    do
        result <- spaceySepBy1 sep parser
        case result of
            Single (WithEol item eol) ->
                return $ \pre post -> [item pre (combine eol post)]

            Multiple (WithEol first firstEol, postFirst) rest (preLast, WithEol last eol) ->
                return $ \preFirst postLast ->
                    concat
                        [ [first preFirst $ combine firstEol postFirst]
                        , fmap (\(Commented pre (WithEol item eol) post) -> item pre $ combine eol post) rest
                        , [last preLast $ combine eol postLast ]
                        ]


-- DEPRECATED: use spaceySepBy1 instead
spaceySepBy1' :: IParser sep -> IParser a -> IParser (Comments -> Comments -> [Commented a])
spaceySepBy1' sep parser =
    spaceySepBy1'' sep ((\x pre post -> Commented pre x post) <$> parser)


commaSep1 :: IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
commaSep1 =
  spaceySepBy1'' comma


commaSep1' :: IParser a -> IParser (Comments -> Comments -> [Commented a])
commaSep1' =
  spaceySepBy1' comma


toSet :: Ord k => (v -> v -> v) -> [Commented (k, v)] -> Map k (Commented v)
toSet merge values =
    let
        merge' (Commented pre1 a post1) (Commented pre2 b post2) =
            Commented (pre1 ++ pre2) (merge a b) (post1 ++ post2)
    in
    foldl (\m (Commented pre (k, v) post) -> insertWith merge' k (Commented pre v post) m) empty values


commaSep1Set' :: Ord k => IParser (k, v) -> (v -> v -> v) -> IParser (Comments -> Comments -> Map k (Commented v))
commaSep1Set' parser merge =
    do
        values <- commaSep1' parser
        return $ \pre post -> toSet merge $ values pre post


commaSep :: IParser (Comments -> Comments -> a) -> IParser (Maybe (Comments -> Comments -> [a]))
commaSep term =
    option Nothing (Just <$> commaSep1 term)


pipeSep1 :: IParser a -> IParser (ExposedCommentedList a)
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


separated :: IParser sep -> IParser e -> IParser (Either e (R.Region, (WithEol e), [(Comments, Comments, e, Maybe String)], Bool))
separated sep expr' =
  let
    subparser =
      do  start <- getMyPosition
          t1 <- expr'
          arrow <- optionMaybe $ try ((,) <$> restOfLine <*> whitespace <* sep)
          case arrow of
            Nothing ->
                return $ \_multiline -> Left t1
            Just (eolT1, preArrow) ->
                do  postArrow <- whitespace
                    t2 <- separated sep expr'
                    end <- getMyPosition
                    case t2 of
                        Right (_, WithEol t2' eolT2, ts, _) ->
                          return $ \multiline -> Right
                            ( R.Region start end
                            , WithEol t1 eolT1
                            , (preArrow, postArrow, t2', eolT2):ts
                            , multiline
                            )
                        Left t2' ->
                          do
                            eol <- restOfLine
                            return $ \multiline -> Right
                              ( R.Region start end
                              , WithEol t1 eolT1
                              , [(preArrow, postArrow, t2', eol)]
                              , multiline)
  in
    (\(f, multiline) -> f $ multilineToBool multiline) <$> trackNewline subparser


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

betwixt :: Char -> Char -> IParser a -> IParser a
betwixt a b c =
  do  _ <- char a
      out <- c
      _ <- char b <?> "a closing '" ++ [b] ++ "'"
      return out


surround :: Char -> Char -> String -> IParser (Comments -> Comments -> Bool -> a) -> IParser a
surround a z name p =
  let
    -- subparser :: IParser (Bool -> a)
    subparser = do
      _ <- char a
      (pre, v, post) <- padded p
      _ <- char z <?> unwords ["a closing", name, show z]
      return $ \multiline -> v pre post multiline
    in
      (\(f, multiline) -> f (multilineToBool multiline)) <$> trackNewline subparser


-- TODO: push the Multiline type further up in the AST and get rid of this
multilineToBool :: Multiline -> Bool
multilineToBool multine =
  case multine of
    SplitAll -> True
    JoinAll -> False


braces :: IParser (Comments -> Comments -> Bool -> a) -> IParser a
braces =
  surround '[' ']' "brace"


parens :: IParser (Comments -> Comments -> Bool -> a) -> IParser a
parens =
  surround '(' ')' "paren"


brackets :: IParser (Comments -> Comments -> Bool -> a) -> IParser a
brackets =
  surround '{' '}' "bracket"


braces' :: IParser a -> IParser a
braces' =
    surround' '[' ']' "brace"


brackets' :: IParser a -> IParser a
brackets' =
    surround' '{' '}' "bracket"


surround' :: Char -> Char -> String -> IParser a -> IParser a
surround' a z name p = do
  _ <- char a
  v <- p
  _ <- char z <?> unwords ["a closing", name, show z]
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
      _ <- char leftDelim
      vs <- sep''
      _ <- char rightDelim
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


accessible :: ElmVersion -> IParser AST.Expression.Expr -> IParser AST.Expression.Expr
accessible elmVersion exprParser =
  do  start <- getMyPosition

      annotatedRootExpr@(A.A _ _rootExpr) <- exprParser

      access <- optionMaybe (try dot <?> "a field access like .name")

      case access of
        Nothing ->
          return annotatedRootExpr

        Just _ ->
          accessible elmVersion $
            do  v <- lowVar elmVersion
                end <- getMyPosition
                return . A.at start end $
                    -- case rootExpr of
                    --   AST.Expression.VarExpr (AST.Variable.VarRef name@(c:_))
                    --     | Char.isUpper c ->
                    --         AST.Expression.VarExpr $ AST.Variable.VarRef (name ++ '.' : v)
                    --   _ ->
                        AST.Expression.Access annotatedRootExpr v


dot :: IParser ()
dot =
  do  _ <- char '.'
      notFollowedBy (char '.')


commentedKeyword :: ElmVersion -> String -> IParser a -> IParser (KeywordCommented a)
commentedKeyword elmVersion word parser =
  do
    pre <- try (whitespace <* reserved elmVersion word)
    post <- whitespace
    value <- parser
    return $ KeywordCommented pre post value


-- ODD COMBINATORS

failure :: String -> IParser String
failure msg = do
  inp <- getInput
  setInput ('x':inp)
  _ <- anyToken
  fail msg


until :: IParser a -> IParser b -> IParser b
until p end =
    go
  where
    go = end <|> (p >> go)


-- BASIC LANGUAGE LITERALS

shader :: IParser String
shader =
  do  _ <- try (string "[glsl|")
      closeShader id


closeShader :: (String -> a) -> IParser a
closeShader builder =
  choice
    [ do  _ <- try (string "|]")
          return (builder "")
    , do  c <- anyChar
          closeShader (builder . (c:))
    ]


sandwich :: Char -> String -> String
sandwich delim s =
  delim : s ++ [delim]


escaped :: Char -> IParser String
escaped delim =
  try $ do
    _ <- char '\\'
    c <- char '\\' <|> char delim
    return ['\\', c]


processAs :: IParser a -> String -> IParser a
processAs processor s =
    calloutParser s processor
  where
    calloutParser :: String -> IParser a -> IParser a
    calloutParser inp p =
      either (fail . show) return (iParse p inp)
