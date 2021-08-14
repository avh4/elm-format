-- This adapter module allows one to interact with the new Elm parser with a
-- parsec-like API
--
-- Historically the Elm compiler and elm-format have shared the same parsing logic
-- and used parsec[1] + indents[2] as parser. Since Elm 0.15 the compiler got
-- its own custom parser[3] and the dependency on parsec was removed. This
-- change was not integrated into elm-format however which has continued using
-- parsec[3]. It is desirable for elm-format to utilize the new parser however, and
-- this module is the first step in making that transition. With this module
-- it has been possible to replace parsec with the new parser without having to
-- rewrite all of the higher level parser, they just interact with the new parser
-- through this module instead.
--
-- 1. https://hackage.haskell.org/package/parsec-3.1.14.0
-- 2. https://hackage.haskell.org/package/indents-0.3.3
-- 3. https://github.com/elm/compiler/blob/94715a520f499591ac6901c8c822bc87cd1af24f/compiler/src/Parse/Primitives.hs


{-# LANGUAGE BangPatterns #-}

module Parse.ParsecAdapter
  -- Text.Parsec.Prim
  ( (<|>)
  , (<?>)
  , lookAhead
  , try
  , many
  , skipMany
  , runParserT
  , getPosition
  , getState
  , updateState
  -- Text.Parsec.Pos
  , SourcePos
  , SourceName
  , sourceLine
  , sourceColumn
  -- Text.Parsec.Error
  , ParseError
  , newErrorUnknown
  , module Parse.ParsecAdapter.Message
  , newErrorUnknown
  , errorPos
  , errorMessages
  -- Text.Parsec.Combinator
  , many1
  , manyTill
  , skipMany1
  , option
  , optionMaybe
  , anyToken
  , choice
  , notFollowedBy
  , between
  , eof
  -- Text.Parsec.Char
  , oneOf
  , space
  , upper
  , lower
  , alphaNum
  , letter
  , digit
  , hexDigit
  , octDigit
  , char
  , anyChar
  , satisfy
  , string
  -- Text.Parsec.Indents
  , runIndent
  , block
  , indented
  , checkIndent
  , withPos
  )
  where

import Parse.ParsecAdapter.Message (Message(..))
import Parse.Primitives (Row, Col)
import qualified Parse.Primitives as EP
import Parse.State (State(..))

import qualified Control.Applicative as Applicative
import Control.Monad (MonadPlus(..), mzero, liftM)
import qualified Control.Monad.Fail as Fail

import Data.List (nub)
import Data.Typeable (Typeable)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8, Word16)
import Data.Char (chr, ord)
import qualified Data.Char as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)




-- Text.Parsec.Prim


unknownError :: EP.Row -> EP.Col -> ParseError
unknownError row col =
  newErrorUnknown "" row col


unexpected :: String -> Parser a
unexpected msg =
  EP.Parser $ \(EP.State _ _ _ _ row col sourceName _) _ _ _ eerr ->
      eerr row col (newErrorMessage (UnExpect msg) sourceName)


type Parser a = EP.Parser ParseError a


instance Applicative.Alternative (EP.Parser ParseError) where
    empty = mzero
    (<|>) = mplus


instance Fail.MonadFail (EP.Parser ParseError) where
    fail = parserFail


parserFail :: String -> Parser a
parserFail msg =
  EP.Parser $ \(EP.State _ _ _ _ row col sourceName _) _ _ _ eerr ->
    eerr row col $ newErrorMessage (Message msg) sourceName


instance MonadPlus (EP.Parser ParseError) where
  mzero = parserZero
  mplus = parserPlus


parserZero :: Parser a
parserZero =
  EP.Parser $ \state _ _ _ eerr ->
    let
      (EP.State _ _ _ _ row col _ _) = state
    in
    eerr row col unknownError


parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (EP.Parser p) (EP.Parser q) =
  EP.Parser $ \s cok eok cerr eerr ->
    let
      meerr r1 c1 toErr1 =
        let
          neerr r2 c2 toErr2 =
            -- This error merging behavior from parsec is really tricky for
            -- me to understand, especially the error positions.
            -- I doubt that I got this 100% correct.
            let
              err = mergeError (toErr1 r1 c1) (toErr2 r2 c2)
              row = fromIntegral $ sourceLine $ errorPos err
              col = fromIntegral $ sourceColumn $ errorPos err
            in
            eerr row col (\_ _ -> err)
        in q s cok eok cerr neerr
    in
    p s cok eok cerr meerr


infixr 1 <|>
infix  0 <?>


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus


-- TODO: Can the implementation be improved?
--
-- It's probable that this behaviour doesn't 100% match the original parsec
-- behaviour. In particular, parsec stores error information in the _ok_
-- continuations as well, why is that? And is it possible to get the same
-- behaviour with the new parser which only stores errors in the _err_
-- continuations.
(<?>) :: Parser a -> String -> Parser a
(<?>) (EP.Parser p) msg =
  EP.Parser $ \s@(EP.State _ _ _ _ _ _ sn _) cok eok cerr eerr ->
    let
      eerr' row col _ =
        eerr row col (newErrorMessage (Expect msg) sn)
    in
    p s cok eok cerr eerr'


lookAhead :: Parser a -> Parser a
lookAhead (EP.Parser p) =
    EP.Parser $ \s _ eok cerr eerr ->
      let
        eok' a _ = eok a s
      in
      p s eok' eok' cerr eerr


try :: Parser a -> Parser a
try (EP.Parser parser) =
  EP.Parser $ \s cok eok _ err ->
    parser s cok eok err err


-- TODO: See if this can be implemented more eloquently
--
-- The many_ helper and the code after the `in` are very similar and it's not
-- obvious how they differ from looking at the code. Is there a way to make this
-- implementation more obvious? Maybe the code in the `in` could be replaced
-- with a single `many_` call?
many :: Parser a -> Parser [a]
many (EP.Parser p) =
  EP.Parser $ \s cok eok cerr _ ->
    let
      many_ acc x s' =
        p
          s'
          (many_ (x:acc))
          parserDoesNotConsumeErr
          cerr
          (\_ _ _ -> cok (reverse (x:acc)) s')
    in
    p
      s
      (many_ [])
      parserDoesNotConsumeErr
      cerr
      (\_ _ _ -> eok [] s)


skipMany ::Parser a -> Parser ()
skipMany (EP.Parser p) =
  EP.Parser $ \s cok _ cerr _ ->
    let
      skipMany_ s' =
        p
          s'
          (\_ -> skipMany_)
          parserDoesNotConsumeErr
          cerr
          (\_ _ _ -> cok () s')
    in
    skipMany_ s


-- Note that causing a runtime crash when using `many` or `skipMany` with a
-- parser that does not consume is the same behaviour as it was with parsec.
parserDoesNotConsumeErr :: a
parserDoesNotConsumeErr = error "Text.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."


-- This function is very similar to `Parse.Primitives.fromByteString`.
runParserT :: Parser a -> State -> SourceName -> String -> Either ParseError a
runParserT (EP.Parser p) (State newline) name source =
  B.accursedUnutterablePerformIO $
    let
      (B.PS fptr offset length) = stringToByteString source
      !pos = plusPtr (unsafeForeignPtrToPtr fptr) offset
      !end = plusPtr pos length
      !result = p (EP.State fptr pos end 1 1 1 name newline) toOk toOk toErr toErr
    in
    do  touchForeignPtr fptr
        return result


toOk :: a -> EP.State -> Either x a
toOk !a _ =
  Right a


toErr :: EP.Row -> EP.Col -> (EP.Row -> EP.Col -> x) -> Either x a
toErr row col toError =
  Left (toError row col)


stringToByteString :: String -> B.ByteString
stringToByteString = B.pack . concatMap encodeChar


-- https://hackage.haskell.org/package/utf8-string-1.0.2/docs/src/Codec.Binary.UTF8.String.html#encodeChar
encodeChar :: Char -> [Word8]
encodeChar = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]


getPosition :: Parser SourcePos
getPosition =
  do  (EP.State _ _ _ _ row col sourceName _) <- getParserState
      return $ newPos sourceName row col


getState :: Parser State
getState =
  do  (EP.State _ _ _ _ _ _ _ newline) <- getParserState
      return (State newline)


updateState :: (State -> State) -> Parser ()
updateState f =
  do  _ <- updateParserState
        (\(EP.State src pos end indent row col sourceName newline) ->
          let
            (State newline') = f (State newline)
          in
          EP.State src pos end indent row col sourceName newline'
        )
      return ()


getParserState :: Parser EP.State
getParserState = updateParserState id


updateParserState :: (EP.State -> EP.State) -> Parser EP.State
updateParserState f =
  EP.Parser $ \s _ eok _ _ -> eok (f s) (f s)



-- Text.Parsec.Pos


type SourceName = String


data SourcePos = SourcePos SourceName !EP.Row !EP.Col


newPos :: SourceName -> EP.Row -> EP.Col -> SourcePos
newPos =
  SourcePos


sourceLine :: SourcePos -> Int
sourceLine (SourcePos _ row _) =
  fromIntegral row


sourceColumn :: SourcePos -> Int
sourceColumn (SourcePos _ _ col) =
  fromIntegral col


instance Show SourcePos where
  show (SourcePos name line column)
    | null name = showLineColumn
    | otherwise = "\"" ++ name ++ "\" " ++ showLineColumn
    where
      showLineColumn    = "(line " ++ show line ++
                          ", column " ++ show column ++
                          ")"
-- Text.Parsec.Error


messageString :: Message -> String
messageString (SysUnExpect s) = s
messageString (UnExpect    s) = s
messageString (Expect      s) = s
messageString (Message     s) = s


data ParseError = ParseError String Row Col [Message]


errorPos :: ParseError -> SourcePos
errorPos (ParseError sourceName row col _) = newPos sourceName row col


errorMessages :: ParseError -> [Message]
errorMessages (ParseError _ _ _ messages) = messages



-- Create parse errors


newErrorMessage :: Message -> String -> Row -> Col -> ParseError
newErrorMessage msg sourceName row col
    = ParseError sourceName row col [msg]


newErrorUnknown :: String -> Row -> Col -> ParseError
newErrorUnknown sourceName row col
    = ParseError sourceName row col []


setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage msg (ParseError sourceName row col msgs)
    = ParseError sourceName row col (msg : filter (msg /=) msgs)


mergeError :: ParseError -> ParseError -> ParseError
mergeError e1@(ParseError sn1 r1 c1 msgs1) e2@(ParseError _ r2 c2 msgs2)
    -- prefer meaningful errors
    | null msgs2 && not (null msgs1) = e1
    | null msgs1 && not (null msgs2) = e2
    | otherwise
    = case (r1, c1) `compare` (r2, c2) of
        -- select the longest match
        EQ -> ParseError sn1 r1 c1 (msgs1 ++ msgs2)
        GT -> e1
        LT -> e2


instance Show ParseError where
    show err
        = show (errorPos err) ++ ":" ++
          showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"
                           (errorMessages err)


showErrorMessages ::
    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map ("\n"++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1) = span ((SysUnExpect "") ==) msgs
      (unExpect,msgs2)    = span ((UnExpect    "") ==) msgs1
      (expect,messages)   = span ((Expect      "") ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages

      -- helpers
      showMany pre msgs3 = case clean (map messageString msgs3) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = separate ", " . clean

      separate   _ []     = ""
      separate   _ [m]    = m
      separate sep (m:ms) = m ++ sep ++ separate sep ms

      clean             = nub . filter (not . null)



-- Text.Parsec.Combinator


choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) mzero ps


many1 :: Parser a -> Parser [a]
many1 p =
  do  x <- p
      xs <- many p
      return (x:xs)


manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end =
  scan
  where
    scan =
      do{ _ <- end; return [] }
      <|>
      do{ x <- p; xs <- scan; return (x:xs) }


skipMany1 :: Parser a -> Parser ()
skipMany1 p =
  do  _ <- p
      skipMany p


option :: a -> Parser a -> Parser a
option x p = p <|> return x


optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (liftM Just p)


anyToken :: Parser Char
anyToken = anyChar


notFollowedBy :: Show a => Parser a -> Parser ()
notFollowedBy p =
  try $ do{ c <- try p; unexpected (show c) } <|> return ()


between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p =
  do{ _ <- open; x <- p; _ <- close; return x }


 --- `eof` makes the parser fail if the entire input hasn't been consumed.
 --- This function sits in an odd position right now because the new parser
 --- (`Parse.Primiteves.fromByteString` and `Parse.Primitives.fromSnippet`)
 --- automatically does this whereas the adapter (`Parse.ParsecAdapter.runParsercT`)
 --- does not.
 ---
 --- I think the solution is to remove the eof behaviour from the new parser,
 --- but we'll see
eof :: Parser ()
eof = notFollowedBy anyToken <?> "end of input"


-- Text.Parsec.Char


oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (\c -> elem c cs)


space :: Parser Char
space = satisfy C.isSpace <?> "space"


upper :: Parser Char
upper = satisfy C.isUpper <?> "uppercase letter"


lower :: Parser Char
lower = satisfy C.isLower <?> "lowercase letter"


alphaNum :: Parser Char
alphaNum = satisfy C.isAlphaNum <?> "letter or digit"


letter :: Parser Char
letter = satisfy C.isAlpha <?> "letter"


digit :: Parser Char
digit = satisfy C.isDigit <?> "digit"


hexDigit :: Parser Char
hexDigit = satisfy C.isHexDigit <?> "hexadecimal digit"


octDigit :: Parser Char
octDigit = satisfy C.isOctDigit <?> "octal digit"


char :: Char -> Parser Char
char c = satisfy (==c) <?> show [c]


anyChar :: Parser Char
anyChar = satisfy (const True)


satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  EP.Parser $ \s@(EP.State _ pos end _ row col sourceName _) cok _ _ eerr ->
    let
      (char, width) = extractChar s

      errEof = newErrorMessage (SysUnExpect "") sourceName

      errExpect = newErrorMessage (SysUnExpect $ [char]) sourceName
    in
    if pos == end then
      eerr row col errEof
    else if f char then
      cok char (updatePos width char s)
    else
      eerr row col errExpect


string :: String -> Parser String
string "" = return ""
string (c:cs) =
  do  _ <- satisfy ((==) c)
      _ <- string cs
      return (c:cs)


updatePos :: Int -> Char -> EP.State -> EP.State
updatePos width c (EP.State src pos end indent row col sourceName newline) =
  let
    (row', col') =
      case c of
        '\n' -> (row + 1, 1)

        -- The parsec docs states that CR increments line just like an LF does,
        -- this is not what happens in the code though,
        -- see: https://github.com/haskell/parsec/issues/129 for details.
        --
        -- Here we've opted for following the behaviour of parsec, and not the
        -- doccumentation even though this behaviour might be considered a bug.
        '\r' -> (row, col + 1)

        -- The parsec behaviour for tabs is to increment to the nearest
        -- 8'th collumn. Shoud we do this as well?
        -- Let's not implement this unless it turns out that elm-format
        -- needs it.
        '\t' -> (row, (col + 8 - ((col-1) `mod` 8)))

        _ -> (row, col + 1)
  in
  EP.State src (plusPtr pos width) end indent row' col' sourceName newline


-- Inspired by https://hackage.haskell.org/package/utf8-string-1.0.2/docs/src/Codec.Binary.UTF8.String.html#decode
--
-- TODO: "Gracefully" crash on incomplete multibyte codepoint
--
-- If there's an incomplete multibyte codepoint at the end of the file this
-- function will attempt to index ´Word8´'s outside the buffer, resulting in
-- some nasty things. While 100% proper handling for utf-8 is not super important
-- (or even desirable) for elm-format, crashing with a descriptive error message
-- instead of indexing outside the buffer might be worth implementing.
--
--  w0, 4 byte char    w1        w2         w3, outside buffer
--          v           v         v          v
-- | ...,  11110xxx,  10xxxxxx,  10xxxxxx | ...
extractChar :: EP.State -> (Char, Int)
extractChar (EP.State _ pos _ _ _ _ _ _) =
  -- 1 byte codepoint
  if w0 < 0xc0 then
    (chr (fromEnum w0), 1)
  -- 2 byte codepoint
  else if w0 < 0xe0 then
    (multi1, 2)
  -- 3 byte codepoint
  else if w0 < 0xf0 then
    (multi_byte [w1, w2] 0xf 0x800, 3)
  -- 4 byte codepoint
  else if w0 < 0xf8 then
    (multi_byte [w1, w2, w3] 0x7 0x10000, 4)
  else
    error "invalid utf-8"
  where
    w0 = EP.unsafeIndex pos
    w1 = EP.unsafeIndex (plusPtr pos 1)
    w2 = EP.unsafeIndex (plusPtr pos 2)
    w3 = EP.unsafeIndex (plusPtr pos 3)

    -- `Codec.Binary.UTF8.String.decode` has this special case function for
    -- a 2 byte codepoint, why is that? Will it behave the same way if we use
    -- the general `multi_byte` instead?
    multi1 =
      if w1 .&. 0xc0 == 0x80 then
        let d = (fromEnum w0 .&. 0x1f) `shiftL` 6 .|. fromEnum (w1 .&. 0x3f)
        in
        if d >= 0x000080 then
          toEnum d
        else
          error "invalid utf-8"
      else
        error "invalid utf-8"

    multi_byte words mask overlong = aux words (fromEnum (w0 .&. mask))
      where
        aux [] acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc
          | otherwise = error "invalid utf-8"

        aux (w:ws) acc
          | w .&. 0xc0 == 0x80 = aux ws
                               $ shiftL acc 6 .|. fromEnum (w .&. 0x3f)
          | otherwise = error "invalid utf-8"



-- Text.Parsec.Indents


-- indents adds additional data onto parsecs `ParsecT` in order to track
-- indentation information. The new parser tracks this information by itself
-- now, which is why this function becomes a no-op.
runIndent :: s -> a -> a
runIndent _ = id


block :: Parser a -> Parser [a]
block p = withPos $ do
    r <- many1 (checkIndent >> p)
    return r


indented :: Parser ()
indented =
  do  (EP.State _ _ _ indent _ col _ _) <- getParserState
      if col <= indent then fail "not indented" else do return ()


checkIndent :: Parser ()
checkIndent =
  do  (EP.State _ _ _ indent _ col _ _) <- getParserState
      if indent == col then return () else fail "indentation doesn't match"


withPos :: Parser a -> Parser a
withPos (EP.Parser p) =
  EP.Parser $ \s@(EP.State _ _ _ indent _ col _ _) cok eok cerr eerr ->
    let
      cok' x s' = cok x (setIndent indent s')
      eok' x s' = eok x (setIndent indent s')
    in
    p (setIndent col s) cok' eok' cerr eerr


setIndent :: Word16 -> EP.State -> EP.State
setIndent indent (EP.State s p e _ r c nl sn) =
  EP.State s p e indent r c nl sn
