{-# LANGUAGE OverloadedStrings #-}
module Cheapskate.Inlines (
        parseInlines
      , pHtmlTag
      , pReference
      , pLinkLabel)
where
import Cheapskate.ParserCombinators
import Cheapskate.Util
import Cheapskate.Types
import Data.Char hiding (Space)
import qualified Data.Sequence as Seq
import Data.Sequence (singleton, (<|), viewl, ViewL(..))
import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

-- Returns tag type and whole tag.
pHtmlTag :: Parser (HtmlTagType, Text)
pHtmlTag = do
  _ <- char '<'
  -- do not end the tag with a > character in a quoted attribute.
  closing <- (char '/' >> return True) <|> return False
  tagname <- takeWhile1 (\c -> isAsciiAlphaNum c || c == '?' || c == '!')
  let tagname' = T.toLower tagname
  let attr = do ss <- takeWhile isSpace
                x <- satisfy isLetter
                xs <- takeWhile (\c -> isAsciiAlphaNum c || c == ':')
                skip (=='=')
                v <- pQuoted '"' <|> pQuoted '\'' <|> takeWhile1 isAlphaNum
                      <|> return ""
                return $ ss <> T.singleton x <> xs <> "=" <> v
  attrs <- T.concat <$> many attr
  final <- takeWhile (\c -> isSpace c || c == '/')
  _ <- char '>'
  let tagtype = if closing
                   then Closing tagname'
                   else case T.stripSuffix "/" final of
                         Just _  -> SelfClosing tagname'
                         Nothing -> Opening tagname'
  return (tagtype,
          T.pack ('<' : ['/' | closing]) <> tagname <> attrs <> final <> ">")

-- Parses a quoted attribute value.
pQuoted :: Char -> Parser Text
pQuoted c = do
  skip (== c)
  contents <- takeTill (== c)
  skip (== c)
  return (T.singleton c <> contents <> T.singleton c)

-- Parses an HTML comment. This isn't really correct to spec, but should
-- do for now.
pHtmlComment :: Parser Text
pHtmlComment = do
  _ <- string "<!--"
  rest <- manyTill anyChar (string "-->")
  return $ "<!--" <> T.pack rest <> "-->"

-- A link label [like this].  Note the precedence:  code backticks have
-- precedence over label bracket markers, which have precedence over
-- *, _, and other inline formatting markers.
-- So, 2 below contains a link while 1 does not:
-- 1. [a link `with a ](/url)` character
-- 2. [a link *with emphasized ](/url) text*
pLinkLabel :: Parser Text
pLinkLabel = char '[' *> (T.concat <$>
  (manyTill (regChunk <|> pEscaped <|> bracketed <|> codeChunk) (char ']')))
  where regChunk = takeWhile1 (\c -> c /='`' && c /='[' && c /=']' && c /='\\')
        codeChunk = snd <$> pCode'
        bracketed = inBrackets <$> pLinkLabel
        inBrackets t = "[" <> t <> "]"

-- A URL in a link or reference.  This may optionally be contained
-- in `<..>`; otherwise whitespace and unbalanced right parentheses
-- aren't allowed.  Newlines aren't allowed in any case.
pLinkUrl :: Parser Text
pLinkUrl = do
  inPointy <- (char '<' >> return True) <|> return False
  if inPointy
     then T.pack <$> manyTill
           (pSatisfy (\c -> c /='\r' && c /='\n')) (char '>')
     else T.concat <$> many (regChunk <|> parenChunk)
    where regChunk = takeWhile1 (notInClass " \n()\\") <|> pEscaped
          parenChunk = parenthesize . T.concat <$> (char '(' *>
                         manyTill (regChunk <|> parenChunk) (char ')'))
          parenthesize x = "(" <> x <> ")"

-- A link title, single or double quoted or in parentheses.
-- Note that Markdown.pl doesn't allow the parenthesized form in
-- inline links -- only in references -- but this restriction seems
-- arbitrary, so we remove it here.
pLinkTitle :: Parser Text
pLinkTitle = do
  c <- satisfy (\c -> c == '"' || c == '\'' || c == '(')
  next <- peekChar
  case next of
       Nothing                 -> mzero
       Just x
         | isWhitespace x      -> mzero
         | x == ')'            -> mzero
         | otherwise           -> return ()
  let ender = if c == '(' then ')' else c
  let pEnder = char ender <* nfb (skip isAlphaNum)
  let regChunk = takeWhile1 (\x -> x /= ender && x /= '\\') <|> pEscaped
  let nestedChunk = (\x -> T.singleton c <> x <> T.singleton ender)
                      <$> pLinkTitle
  T.concat <$> manyTill (regChunk <|> nestedChunk) pEnder

-- A link reference is a square-bracketed link label, a colon,
-- optional space or newline, a URL, optional space or newline,
-- and an optional link title.  (Note:  we assume the input is
-- pre-stripped, with no leading/trailing spaces.)
pReference :: Parser (Text, Text, Text)
pReference = do
  lab <- pLinkLabel
  _ <- char ':'
  scanSpnl
  url <- pLinkUrl
  tit <- option T.empty $ scanSpnl >> pLinkTitle
  endOfInput
  return (lab, url, tit)

-- Parses an escaped character and returns a Text.
pEscaped :: Parser Text
pEscaped = T.singleton <$> (skip (=='\\') *> satisfy isEscapable)

-- Parses a (possibly escaped) character satisfying the predicate.
pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy p =
  satisfy (\c -> c /= '\\' && p c)
   <|> (char '\\' *> satisfy (\c -> isEscapable c && p c))

-- Parse a text into inlines, resolving reference links
-- using the reference map.
parseInlines :: ReferenceMap -> Text -> Inlines
parseInlines refmap t =
  case parse (msum <$> many (pInline refmap) <* endOfInput) t of
       Left e   -> error ("parseInlines: " ++ show e) -- should not happen
       Right r  -> r

pInline :: ReferenceMap -> Parser Inlines
pInline refmap =
           pAsciiStr
       <|> pSpace
       <|> pEnclosure '*' refmap  -- strong/emph
       <|> (notAfter isAlphaNum *> pEnclosure '_' refmap)
       <|> pCode
       <|> pLink refmap
       <|> pImage refmap
       <|> pRawHtml
       <|> pAutolink
       <|> pEntity
       <|> pSym

-- Parse spaces or newlines, and determine whether
-- we have a regular space, a line break (two spaces before
-- a newline), or a soft break (newline without two spaces
-- before).
pSpace :: Parser Inlines
pSpace = do
  ss <- takeWhile1 isWhitespace
  return $ singleton
         $ if T.any (=='\n') ss
              then if "  " `T.isPrefixOf` ss
                   then LineBreak
                   else SoftBreak
              else Space

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9')

pAsciiStr :: Parser Inlines
pAsciiStr = do
  t <- takeWhile1 isAsciiAlphaNum
  mbc <- peekChar
  case mbc of
       Just ':' -> if t `Set.member` schemeSet
                      then pUri t
                      else return $ singleton $ Str t
       _        -> return $ singleton $ Str t

-- Catch all -- parse an escaped character, an escaped
-- newline, or any remaining symbol character.
pSym :: Parser Inlines
pSym = do
  c <- anyChar
  let ch = singleton . Str . T.singleton
  if c == '\\'
     then ch <$> satisfy isEscapable
          <|> singleton LineBreak <$ satisfy (=='\n')
          <|> return (ch '\\')
     else return (ch c)

-- http://www.iana.org/assignments/uri-schemes.html plus
-- the unofficial schemes coap, doi, javascript.
schemes :: [Text]
schemes = [ -- unofficial
            "coap","doi","javascript"
           -- official
           ,"aaa","aaas","about","acap"
           ,"cap","cid","crid","data","dav","dict","dns","file","ftp"
           ,"geo","go","gopher","h323","http","https","iax","icap","im"
           ,"imap","info","ipp","iris","iris.beep","iris.xpc","iris.xpcs"
           ,"iris.lwz","ldap","mailto","mid","msrp","msrps","mtqp"
           ,"mupdate","news","nfs","ni","nih","nntp","opaquelocktoken","pop"
           ,"pres","rtsp","service","session","shttp","sieve","sip","sips"
           ,"sms","snmp","soap.beep","soap.beeps","tag","tel","telnet","tftp"
           ,"thismessage","tn3270","tip","tv","urn","vemmi","ws","wss"
           ,"xcon","xcon-userid","xmlrpc.beep","xmlrpc.beeps","xmpp","z39.50r"
           ,"z39.50s"
           -- provisional
           ,"adiumxtra","afp","afs","aim","apt","attachment","aw"
           ,"beshare","bitcoin","bolo","callto","chrome","chrome-extension"
           ,"com-eventbrite-attendee","content","cvs","dlna-playsingle"
           ,"dlna-playcontainer","dtn","dvb","ed2k","facetime","feed"
           ,"finger","fish","gg","git","gizmoproject","gtalk"
           ,"hcp","icon","ipn","irc","irc6","ircs","itms","jar"
           ,"jms","keyparc","lastfm","ldaps","magnet","maps","market"
           ,"message","mms","ms-help","msnim","mumble","mvn","notes"
           ,"oid","palm","paparazzi","platform","proxy","psyc","query"
           ,"res","resource","rmi","rsync","rtmp","secondlife","sftp"
           ,"sgn","skype","smb","soldat","spotify","ssh","steam","svn"
           ,"teamspeak","things","udp","unreal","ut2004","ventrilo"
           ,"view-source","webcal","wtai","wyciwyg","xfire","xri"
           ,"ymsgr" ]

-- Make them a set for more efficient lookup.
schemeSet :: Set.Set Text
schemeSet = Set.fromList $ schemes ++ map T.toUpper schemes

-- Parse a URI, using heuristics to avoid capturing final punctuation.
pUri :: Text -> Parser Inlines
pUri scheme = do
  _ <- char ':'
  x <- scan (OpenParens 0) uriScanner
  guard $ not $ T.null x
  let (rawuri, endingpunct) =
        case T.last x of
             c | c `elem` (".;?!:," :: String) ->
               (scheme <> ":" <> T.init x, singleton (Str (T.singleton c)))
             _ -> (scheme <> ":" <> x, mempty)
  return $ autoLink rawuri <> endingpunct

-- Scan non-ascii characters and ascii characters allowed in a URI.
-- We allow punctuation except when followed by a space, since
-- we don't want the trailing '.' in 'http://google.com.'
-- We want to allow
-- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
-- as a URL, while NOT picking up the closing paren in
-- (http://wikipedia.org)
-- So we include balanced parens in the URL.

data OpenParens = OpenParens Int

uriScanner :: OpenParens -> Char -> Maybe OpenParens
uriScanner _ ' '  = Nothing
uriScanner _ '\n' = Nothing
uriScanner (OpenParens n) '(' = Just (OpenParens (n + 1))
uriScanner (OpenParens n) ')'
  | n > 0 = Just (OpenParens (n - 1))
  | otherwise = Nothing
uriScanner st '+' = Just st
uriScanner st '/' = Just st
uriScanner _ c | isSpace c = Nothing
uriScanner st _ = Just st

-- Parses material enclosed in *s, **s, _s, or __s.
-- Designed to avoid backtracking.
pEnclosure :: Char -> ReferenceMap -> Parser Inlines
pEnclosure c refmap = do
  cs <- takeWhile1 (== c)
  (Str cs <|) <$> pSpace
   <|> case T.length cs of
            3  -> pThree c refmap
            2  -> pTwo c refmap mempty
            1  -> pOne c refmap mempty
            _  -> return (singleton $ Str cs)

-- singleton sequence or empty if contents are empty
single :: (Inlines -> Inline) -> Inlines -> Inlines
single constructor ils = if Seq.null ils
                            then mempty
                            else singleton (constructor ils)

-- parse inlines til you hit a c, and emit Emph.
-- if you never hit a c, emit '*' + inlines parsed.
pOne :: Char -> ReferenceMap -> Inlines -> Parser Inlines
pOne c refmap prefix = do
  contents <- msum <$> many ( (nfbChar c >> pInline refmap)
                             <|> (string (T.pack [c,c]) >>
                                  nfbChar c >> pTwo c refmap mempty) )
  (char c >> return (single Emph $ prefix <> contents))
    <|> return (singleton (Str (T.singleton c)) <> (prefix <> contents))

-- parse inlines til you hit two c's, and emit Strong.
-- if you never do hit two c's, emit '**' plus + inlines parsed.
pTwo :: Char -> ReferenceMap -> Inlines -> Parser Inlines
pTwo c refmap prefix = do
  let ender = string $ T.pack [c,c]
  contents <- msum <$> many (nfb ender >> pInline refmap)
  (ender >> return (single Strong $ prefix <> contents))
    <|> return (singleton (Str $ T.pack [c,c]) <> (prefix <> contents))

-- parse inlines til you hit one c or a sequence of two c's.
-- If one c, emit Emph and then parse pTwo.
-- if two c's, emit Strong and then parse pOne.
pThree :: Char -> ReferenceMap -> Parser Inlines
pThree c refmap = do
  contents <- msum <$> (many (nfbChar c >> pInline refmap))
  (string (T.pack [c,c]) >> (pOne c refmap (single Strong contents)))
   <|> (char c >> (pTwo c refmap (single Emph contents)))
   <|> return (singleton (Str $ T.pack [c,c,c]) <> contents)

-- Inline code span.
pCode :: Parser Inlines
pCode = fst <$> pCode'

-- this is factored out because it needed in pLinkLabel.
pCode' :: Parser (Inlines, Text)
pCode' = do
  ticks <- takeWhile1 (== '`')
  let end = string ticks >> nfb (char '`')
  let nonBacktickSpan = takeWhile1 (/= '`')
  let backtickSpan = takeWhile1 (== '`')
  contents <- T.concat <$> manyTill (nonBacktickSpan <|> backtickSpan) end
  return (singleton . Code . T.strip $ contents, ticks <> contents <> ticks)

pLink :: ReferenceMap -> Parser Inlines
pLink refmap = do
  lab <- pLinkLabel
  let lab' = parseInlines refmap lab
  pInlineLink lab' <|> pReferenceLink refmap lab lab'
    -- fallback without backtracking if it's not a link:
    <|> return (singleton (Str "[") <> lab' <> singleton (Str "]"))

-- An inline link: [label](/url "optional title")
pInlineLink :: Inlines -> Parser Inlines
pInlineLink lab = do
  _ <- char '('
  scanSpaces
  url <- pLinkUrl
  tit <- option "" $ scanSpnl *> pLinkTitle <* scanSpaces
  _ <- char ')'
  return $ singleton $ Link lab (Url url) tit

-- A reference link: [label], [foo][label], or [label][].
pReferenceLink :: ReferenceMap -> Text -> Inlines -> Parser Inlines
pReferenceLink _ rawlab lab = do
  ref <- option rawlab $ scanSpnl >> pLinkLabel
  return $ singleton $ Link lab (Ref ref) ""


-- An image:  ! followed by a link.
pImage :: ReferenceMap -> Parser Inlines
pImage refmap = do
  _ <- char '!'
  (linkToImage <$> pLink refmap) <|> return (singleton (Str "!"))

linkToImage :: Inlines -> Inlines
linkToImage ils =
  case viewl ils of
        (Link lab (Url url) tit :< x)
          | Seq.null x -> singleton (Image lab url tit)
        _ -> singleton (Str "!") <> ils

-- An entity.  We store these in a special inline element.
-- This ensures that entities in the input come out as
-- entities in the output. Alternatively we could simply
-- convert them to characters and store them as Str inlines.
pEntity :: Parser Inlines
pEntity = do
  _ <- char '&'
  res <- pCharEntity <|> pDecEntity <|> pHexEntity
  _ <- char ';'
  return $ singleton $ Entity $ "&" <> res <> ";"

pCharEntity :: Parser Text
pCharEntity = takeWhile1 (\c -> isAscii c && isLetter c)

pDecEntity :: Parser Text
pDecEntity = do
  _ <- char '#'
  res <- takeWhile1 isDigit
  return $ "#" <> res

pHexEntity :: Parser Text
pHexEntity = do
  _ <- char '#'
  x <- char 'X' <|> char 'x'
  res <- takeWhile1 isHexDigit
  return $ "#" <> T.singleton x <> res

-- Raw HTML tag or comment.
pRawHtml :: Parser Inlines
pRawHtml = singleton . RawHtml <$> (snd <$> pHtmlTag <|> pHtmlComment)

-- A link like this: <http://whatever.com> or <me@mydomain.edu>.
-- Markdown.pl does email obfuscation; we don't bother with that here.
pAutolink :: Parser Inlines
pAutolink = do
  skip (=='<')
  s <- takeWhile1 (\c -> c /= ':' && c /= '@')
  rest <- takeWhile1 (\c -> c /='>' && c /= ' ')
  skip (=='>')
  case True of
       _ | "@" `T.isPrefixOf` rest -> return $ emailLink (s <> rest)
         | s `Set.member` schemeSet -> return $ autoLink (s <> rest)
         | otherwise   -> fail "Unknown contents of <>"

autoLink :: Text -> Inlines
autoLink t = singleton $ Link (toInlines t) (Url t) (T.empty)
  where toInlines t' = case parse pToInlines t' of
                         Right r   -> r
                         Left e    -> error $ "autolink: " ++ show e
        pToInlines = mconcat <$> many strOrEntity
        strOrEntity = ((singleton . Str) <$> takeWhile1 (/='&'))
                   <|> pEntity
                   <|> ((singleton . Str) <$> string "&")

emailLink :: Text -> Inlines
emailLink t = singleton $ Link (singleton $ Str t)
                               (Url $ "mailto:" <> t) (T.empty)
