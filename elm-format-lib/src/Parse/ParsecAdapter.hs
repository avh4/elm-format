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
  , parserFail
  , lookAhead
  , try
  , many
  , skipMany
  , runParserT
  , getPosition
  , getState
  , updateState
  -- Text.Parsec.Error
  , Reporting.Error.Syntax.ParsecError(..)
  , Reporting.Error.Syntax.Message(..)
  , parseError
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

import Parse.Primitives (Row, Col)
import qualified Parse.Primitives as P
import Parse.State (State(..))

import qualified Reporting.Annotation as A
import Reporting.Error.Syntax (ParsecError(..), Message(..))

import qualified Control.Applicative as Applicative

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word (Word8, Word16)
import Data.Char (chr, ord)
import qualified Data.Char as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Foreign.Ptr (plusPtr)
import Foreign.ForeignPtr (withForeignPtr)

import System.IO.Unsafe (unsafePerformIO)




-- Text.Parsec.Prim


type Parser a = P.Parser ParsecError a


instance Applicative.Alternative (P.Parser ParsecError) where
    empty = parserFail $ parseError (Message "Parse.ParsecAdapter.empty")
    (<|>) = (<|>)


parserFail :: (Row -> Col -> x) -> P.Parser x a
parserFail toErr =
  P.Parser $ \(P.State _ _ _ _ row col _) _ _ _ eerr ->
    eerr row col toErr


infixr 1 <|>
infix  0 <?>


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (P.Parser p) (P.Parser q) =
  P.Parser $ \s cok eok cerr eerr ->
    let
      meerr r1 c1 toErr1 =
        let
          neerr r2 c2 toErr2 =
            let
              err = mergeError (toErr1 r1 c1) (toErr2 r2 c2)
              (A.Position row col) = errorPos err
            in
            eerr row col (\_ _ -> err)
        in q s cok eok cerr neerr
    in
    p s cok eok cerr meerr


(<?>) :: Parser a -> String -> Parser a
(<?>) (P.Parser p) msg =
  P.Parser $ \s cok eok cerr eerr ->
    let
      eerr' row col err =
        eerr row col $ addMessage (Expect msg) err
    in
    p s cok eok cerr eerr'


lookAhead :: Parser a -> Parser a
lookAhead (P.Parser p) =
    P.Parser $ \s _ eok cerr eerr ->
      let
        eok' a _ = eok a s
      in
      p s eok' eok' cerr eerr


try :: Parser a -> Parser a
try (P.Parser parser) =
  P.Parser $ \s cok eok _ err ->
    parser s cok eok err err


-- TODO: See if this can be implemented more eloquently
--
-- The many_ helper and the code after the `in` are very similar and it's not
-- obvious how they differ from looking at the code. Is there a way to make this
-- implementation more obvious? Maybe the code in the `in` could be replaced
-- with a single `many_` call?
many :: Parser a -> Parser [a]
many (P.Parser p) =
  P.Parser $ \s cok eok cerr _ ->
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
skipMany (P.Parser p) =
  P.Parser $ \s cok _ cerr _ ->
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
runParserT :: Parser a -> State -> String -> Either ParsecError a
runParserT (P.Parser p) (State newline) source =
  unsafePerformIO $
    let
      (B.PS fptr offset length) = stringToByteString source

      run' ptr =
        let
          !pos = plusPtr ptr offset
          !end = plusPtr pos length
          !result = p (P.State fptr pos end 1 1 1 newline) toOk toOk toErr toErr
        in
        return result
    in
    withForeignPtr fptr run'


toOk :: a -> P.State -> Either x a
toOk !a _ =
  Right a


toErr :: P.Row -> P.Col -> (P.Row -> P.Col -> x) -> Either x a
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


getPosition :: Parser A.Position
getPosition =
  do  (P.State _ _ _ _ row col _) <- getParserState
      return $ A.Position row col


getState :: Parser State
getState =
  do  (P.State _ _ _ _ _ _ newline) <- getParserState
      return (State newline)


updateState :: (State -> State) -> Parser ()
updateState f =
  do  _ <- updateParserState
        (\(P.State src pos end indent row col newline) ->
          let
            (State newline') = f (State newline)
          in
          P.State src pos end indent row col newline'
        )
      return ()


getParserState :: Parser P.State
getParserState = updateParserState id


updateParserState :: (P.State -> P.State) -> Parser P.State
updateParserState f =
  P.Parser $ \s _ eok _ _ -> eok (f s) (f s)



-- Text.Parsec.Error


parseError :: Message -> Row -> Col -> ParsecError
parseError message row col =
  Cons message row col Nil


addMessage :: Message -> (Row -> Col -> ParsecError) -> Row -> Col -> ParsecError
addMessage message toErr row col =
  Cons message row col (toErr row col)


mergeError :: ParsecError -> ParsecError -> ParsecError
mergeError = OneOf


errorPos :: ParsecError -> A.Position
errorPos err =
  case err of
    Nil ->
      error "An unexpeced error occured, this is likely a bug. Please report this issue at https://github.com/avh4/elm-format/issues"

    Cons _ row col _ ->
      A.Position row col

    OneOf _ e2 ->
      errorPos e2


errorMessages :: ParsecError -> [Message]
errorMessages err =
  case err of
    Nil ->
      []

    Cons message _ _ subErrs ->
      message : errorMessages subErrs

    OneOf _ e2 ->
      errorMessages e2



-- Text.Parsec.Combinator


choice :: [Parser a] -> Parser a
choice ps =
  let
    err = parseError (Message "Parse.ParsecAdapter.choice")
  in
  foldr (<|>) (parserFail err) ps


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
optionMaybe p = option Nothing (fmap Just p)


anyToken :: Parser Char
anyToken = anyChar


notFollowedBy :: Show a => Parser a -> Parser ()
notFollowedBy p =
  try $
    do{ c <- try p;
        parserFail $ parseError (UnExpect (show c))
      }
    <|> return ()


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
  P.Parser $ \s@(P.State _ pos end _ row col _) cok _ _ eerr ->
    let
      (char, width) = extractChar s

      errEof = parseError (UnExpect "end of file")

      errExpect = parseError (UnExpect [char])
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


updatePos :: Int -> Char -> P.State -> P.State
updatePos width c (P.State src pos end indent row col newline) =
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
  P.State src (plusPtr pos width) end indent row' col' newline


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
extractChar :: P.State -> (Char, Int)
extractChar (P.State _ pos _ _ _ _ _) =
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
    w0 = P.unsafeIndex pos
    w1 = P.unsafeIndex (plusPtr pos 1)
    w2 = P.unsafeIndex (plusPtr pos 2)
    w3 = P.unsafeIndex (plusPtr pos 3)

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
runIndent :: a -> a
runIndent = id


block :: Parser a -> Parser [a]
block p = withPos $ do
    r <- many1 (checkIndent >> p)
    return r


indented :: Parser ()
indented =
  do  (P.State _ _ _ indent _ col _) <- getParserState
      if col <= indent then
        parserFail $ parseError (Message "not indented")
      else do
        return ()


checkIndent :: Parser ()
checkIndent =
  do  (P.State _ _ _ indent _ col _) <- getParserState
      if indent == col then
        return ()
      else
        parserFail $ parseError (Message "indentation doesn't match")


withPos :: Parser a -> Parser a
withPos (P.Parser p) =
  P.Parser $ \s@(P.State _ _ _ indent _ col _) cok eok cerr eerr ->
    let
      cok' x s' = cok x (setIndent indent s')
      eok' x s' = eok x (setIndent indent s')
    in
    p (setIndent col s) cok' eok' cerr eerr


setIndent :: Word16 -> P.State -> P.State
setIndent indent (P.State s p e _ r c nl) =
  P.State s p e indent r c nl
