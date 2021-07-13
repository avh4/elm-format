{-# LANGUAGE CPP, FlexibleContexts, Rank2Types #-}

module Text.Parsec.Char
  ( oneOf
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
  ) where

import Parse.Primitives (Row, Col, State)
import qualified Parse.Primitives as EP
import Text.Parsec.Prim (Parser, (<?>))
import Text.Parsec.Error (ParseError, Message(SysUnExpect, Expect), newErrorMessage, setErrorMessage)

import Foreign.Ptr (plusPtr)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.Word (Word8)
import Data.Char (chr, ord)
import qualified Data.Char as C


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
string match@(c:cs) =
  EP.Parser $ \s cok _ cerr eerr ->
    stringHelp
      [c]
      s
      (\s' ->
        stringHelp
          cs
          s'
          (cok match)
          cerr
      )
      eerr


stringHelp :: forall b.
  String
  -> EP.State
  -> (State -> b)
  -> (Row -> Col -> (Row -> Col -> ParseError) -> b)
  -> b
stringHelp "" s toOk _ = toOk s
stringHelp (c:cs) s@(EP.State _ pos end _ row col sourceName _) toOk toError =
  let
    errEof _ _ = setErrorMessage (Expect (show (c:cs)))
                  (newErrorMessage (SysUnExpect "") sourceName row col)

    errExpect x _ _ = setErrorMessage (Expect (show (c:cs)))
                        (newErrorMessage (SysUnExpect (show x)) sourceName row col)

    (char, width) = extractChar s
  in
  if pos == end then
    toError row col errEof
  else if char == c then
    stringHelp cs (updatePos width char s) toOk toError
  else
    toError row col (errExpect c)


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
        -- Let's follow the parsec behaviour
        '\t' -> (row, (col + 8 - ((col-1) `mod` 8)))

        _ -> (row, col + 1)
  in
  EP.State src (plusPtr pos width) end indent row' col' sourceName newline


-- Inspired by https://hackage.haskell.org/package/utf8-string-1.0.2/docs/src/Codec.Binary.UTF8.String.html#decode
extractChar :: EP.State -> (Char, Int)
extractChar (EP.State _ pos _ _ _ _ _ _) =
  if w0 < 0xc0 then
    (chr (fromEnum w0), 1)
  else if w0 < 0xe0 then
    (multi1, 2)
  else if w0 < 0xf0 then
    (multi_byte [w1, w2] 0xf 0x800, 3)
  else if w0 < 0xf8 then
    (multi_byte [w1, w2, w3] 0x7 0x10000, 4)
  else
    error "invalid utf-8"
  where
    w0 = EP.unsafeIndex pos
    w1 = EP.unsafeIndex (plusPtr pos 1)
    w2 = EP.unsafeIndex (plusPtr pos 2)
    w3 = EP.unsafeIndex (plusPtr pos 3)

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
