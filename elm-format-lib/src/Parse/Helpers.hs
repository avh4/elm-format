{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Parse.Helpers where

import Prelude hiding (until)
import Control.Monad (guard)
import qualified Data.Indexed as I
import Data.Map.Strict hiding (foldl)
import Parse.ParsecAdapter hiding (newline, spaces, State)

import AST.V0_16
import qualified AST.Helpers as Help
import AST.Structure (FixAST)
import ElmVersion
import qualified Parse.State as State
import Parse.Comments
import Parse.IParser
import Parse.Whitespace
import qualified Parse.Primitives as P
import qualified Parse.ParsecAdapter as Parsec
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as Syntax


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

iParse :: IParser a -> String -> Either ParsecError a
iParse =
    iParseWithState State.init


iParseWithState :: State.State -> IParser a -> String -> Either ParsecError a
iParseWithState state aParser input =
  runIndent $ runParserT aParser state input


-- VARIABLES

var :: ElmVersion -> IParser (Ref [UppercaseIdentifier])
var elmVersion =
  try (qualifiedVar elmVersion) <|> qualifiedTag elmVersion <?> "a name"


lowVar :: ElmVersion -> IParser LowercaseIdentifier
lowVar elmVersion =
  LowercaseIdentifier <$> makeVar elmVersion lower <?> "a lower case name"


capVar :: ElmVersion -> IParser UppercaseIdentifier
capVar elmVersion =
  UppercaseIdentifier <$> makeVar elmVersion upper <?> "an upper case name"


qualifiedVar :: ElmVersion -> IParser (Ref [UppercaseIdentifier])
qualifiedVar elmVersion =
    VarRef
        <$> many (const <$> capVar elmVersion <*> string ".")
        <*> lowVar elmVersion


qualifiedTag :: ElmVersion -> IParser (Ref [UppercaseIdentifier])
qualifiedTag elmVersion =
    TagRef
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
        then parserFail $ parseError (Message (Syntax.keyword variable))
        else return variable


reserved :: ElmVersion -> String -> IParser ()
reserved elmVersion word =
  expecting ("reserved word `" ++ word ++ "`") $
    do  _ <- string word
        notFollowedBy (innerVarChar elmVersion)
        return ()


-- INFIX OPERATORS

anyOp :: ElmVersion -> IParser (Ref [UppercaseIdentifier])
anyOp elmVersion =
  (betwixt '`' '`' (qualifiedVar elmVersion) <?> "an infix operator like `andThen`")
  <|> (OpRef <$> symOp)


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
                (C eol next) <- withEol parser
                choice
                    [ try (padded sep)
                        >>= (\(C (preSep, postSep) _) -> step first (C (post, preSep, eol) next : rest) postSep)
                    , return $ Multiple first (reverse rest) (C (post, eol) next)
                    ]

    in
        do
            (C eol value) <- withEol parser
            choice
                [ try (padded sep)
                    >>= (\(C (preSep, postSep) _) -> step (C (preSep, eol) value) [] postSep)
                , return $ Single (C eol value)
                ]


-- DEPRECATED: use spaceySepBy1 instead
spaceySepBy1'' :: IParser sep -> IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
spaceySepBy1'' sep parser =
  let
    combine eol post =
      eolToComment eol ++ post
  in
    do
        result <- spaceySepBy1 sep parser
        case result of
            Single (C eol item) ->
                return $ \pre post -> [item pre (combine eol post)]

            Multiple (C (postFirst, firstEol) first ) rest (C (preLast, eol) last) ->
                return $ \preFirst postLast ->
                    concat
                        [ [first preFirst $ combine firstEol postFirst]
                        , fmap (\(C (pre, post, eol) item) -> item pre $ combine eol post) rest
                        , [last preLast $ combine eol postLast ]
                        ]


-- DEPRECATED: use spaceySepBy1 instead
spaceySepBy1' :: IParser sep -> IParser a -> IParser (Comments -> Comments -> [C2 before after a])
spaceySepBy1' sep parser =
    spaceySepBy1'' sep ((\x pre post -> C (pre, post) x) <$> parser)


commaSep1 :: IParser (Comments -> Comments -> a) -> IParser (Comments -> Comments -> [a])
commaSep1 =
  spaceySepBy1'' comma


commaSep1' :: IParser a -> IParser (Comments -> Comments -> [C2 before after a])
commaSep1' =
  spaceySepBy1' comma


toSet :: Ord k => (v -> v -> v) -> [C2 before after (k, v)] -> Map k (C2 before after v)
toSet merge values =
    let
        merge' (C (pre1, post1) a) (C (pre2, post2) b) =
            C (pre1 ++ pre2, post1 ++ post2) (merge a b)
    in
    foldl (\m (C (pre, post) (k, v)) -> insertWith merge' k (C (pre, post) v) m) empty values


commaSep1Set' :: Ord k => IParser (k, v) -> (v -> v -> v) -> IParser (Comments -> Comments -> Map k (C2 before after v))
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


keyValue :: IParser sep -> IParser key -> IParser val -> IParser (Comments -> Comments -> (C2 before after key, C2 before' after' val) )
keyValue parseSep parseKey parseVal =
  do
    key <- parseKey
    preSep <- whitespace <* parseSep
    postSep <- whitespace
    val <- parseVal
    return $ \pre post ->
      ( C (pre, preSep) key
      , C (postSep, post) val
      )


separated :: IParser sep -> IParser e -> IParser (Either e (A.Region, C0Eol e, Sequence e, Bool))
separated sep expr' =
  let
    subparser =
      do  start <- Parsec.getPosition
          t1 <- expr'
          arrow <- optionMaybe $ try ((,) <$> restOfLine <*> whitespace <* sep)
          case arrow of
            Nothing ->
                return $ \_multiline -> Left t1
            Just (eolT1, preArrow) ->
                do  postArrow <- whitespace
                    t2 <- separated sep expr'
                    end <- Parsec.getPosition
                    case t2 of
                        Right (_, C eolT2 t2', Sequence ts, _) ->
                          return $ \multiline -> Right
                            ( A.Region start end
                            , C eolT1 t1
                            , Sequence (C (preArrow, postArrow, eolT2) t2' : ts)
                            , multiline
                            )
                        Left t2' ->
                          do
                            eol <- restOfLine
                            return $ \multiline -> Right
                              ( A.Region start end
                              , C eolT1 t1
                              , Sequence [ C (preArrow, postArrow, eol) t2' ]
                              , multiline)
  in
    (\(f, multiline) -> f $ multilineToBool multiline) <$> trackNewline subparser


dotSep1 :: IParser a -> IParser [a]
dotSep1 p =
  (:) <$> p <*> many (try (char '.') >> p)


spacePrefix :: IParser a -> IParser [C1 before a]
spacePrefix p =
  fmap fst <$>
      constrainedSpacePrefix' p (\_ -> return ())


constrainedSpacePrefix :: IParser a -> IParser [(C1 before a, Multiline)]
constrainedSpacePrefix parser =
  constrainedSpacePrefix' parser constraint
  where
    constraint empty = if empty then notFollowedBy (char '-') else return ()


constrainedSpacePrefix' :: IParser a -> (Bool -> IParser b) -> IParser [(C1 before a, Multiline)]
constrainedSpacePrefix' parser constraint =
    many $ trackNewline $ choice
      [ C <$> try (const <$> spacing <*> lookAhead (oneOf "[({")) <*> parser
      , try (C <$> spacing <*> parser)
      ]
    where
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
      (C (pre, post) v) <- padded p
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


parens'' :: IParser a -> IParser (Either Comments [C2 before after a])
parens'' = surround'' '(' ')'


braces'' :: IParser a -> IParser (Either Comments [C2 before after a])
braces'' = surround'' '[' ']'


surround'' :: Char -> Char -> IParser a -> IParser (Either Comments [C2 before after a])
surround'' leftDelim rightDelim inner =
  let
    sep''' =
      do
        v <- (\pre a post -> C (pre, post) a) <$> whitespace <*> inner <*> whitespace
        option [v] ((\x -> v : x) <$> (char ',' >> sep'''))
    sep'' =
      do
          pre <- whitespace
          v <- optionMaybe ((\a post -> C (pre, post) a) <$> inner <*> whitespace)
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

addLocation :: IParser a -> IParser (A.Located a)
addLocation expr =
  do  (start, e, end) <- located expr
      return (A.at start end e)


located :: IParser a -> IParser (A.Position, a, A.Position)
located parser =
  do  start <- Parsec.getPosition
      value <- parser
      end <- Parsec.getPosition
      return (start, value, end)


accessible :: ElmVersion -> IParser (FixAST A.Located typeRef ctorRef varRef 'ExpressionNK) -> IParser (FixAST A.Located typeRef ctorRef varRef 'ExpressionNK)
accessible elmVersion exprParser =
  do  start <- Parsec.getPosition
      rootExpr <- exprParser
      access <- optionMaybe (try dot <?> "a field access like .name")

      case access of
        Nothing ->
          return rootExpr

        Just _ ->
          accessible elmVersion $
            do  v <- lowVar elmVersion
                end <- Parsec.getPosition
                return $ I.Fix $ A.at start end $ Access rootExpr v


dot :: IParser ()
dot =
  do  _ <- char '.'
      notFollowedBy (char '.')


commentedKeyword :: ElmVersion -> String -> IParser a -> IParser (C2 beforeKeyword afterKeyword a)
commentedKeyword elmVersion word parser =
  do
    pre <- try (whitespace <* reserved elmVersion word)
    post <- whitespace
    value <- parser
    return $ C (pre, post) value


-- ODD COMBINATORS

-- Behaves the same as `Parse.ParsecAdapter.fail` except that the consumed
-- continuation is called instead of the empty continuation.
failure :: String -> IParser String
failure msg =
  P.Parser $ \s _ _ cerr _ ->
    let
      (P.Parser p) = parserFail $ parseError (Message msg)
    in
    -- This looks really unsound, but `p` which was created with `fail` will
    -- only ever call the empty error continuation (which in this case
    -- re-routes to the consumed error continuation)
    p s undefined undefined undefined cerr


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
      either (parserFail . const . const) return (iParse p inp)
