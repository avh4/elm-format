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
import Text.Parsec.Prim (Parser(..), (<?>))
import Text.Parsec.Error (ParseError, Message(SysUnExpect, Expect), newErrorMessage, setErrorMessage)

import Foreign.Ptr (plusPtr)
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
lower = satisfy C.isUpper <?> "lowercase letter"


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
  Parser $ EP.Parser $ \s@(EP.State _ pos end _ row col) cok _ _ eerr ->
    let
      w = EP.unsafeIndex pos

      errEof = newErrorMessage (SysUnExpect "") "TODO"

      errExpect = newErrorMessage (SysUnExpect $ show w) "TODO"
    in
    if pos == end then
      eerr row col errEof
    else if w > 127 then
      error "can't handle unicode"
    else if f (chr $ fromEnum w) then
      cok (chr $ fromEnum w) (updatePos w s)
    else
      eerr row col errExpect


string :: String -> Parser String
string "" = return ""
string match@(_:cs) =
  Parser $ EP.Parser $ \s cok _ cerr eerr ->
    stringHelp
      match
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
stringHelp (c:cs) s@(EP.State _ pos end _ row col) toOk toError =
  let
    errEof _ _ = setErrorMessage (Expect (show (c:cs)))
                  (newErrorMessage (SysUnExpect "") "TODO" row col)

    errExpect x _ _ = setErrorMessage (Expect (show (c:cs)))
                        (newErrorMessage (SysUnExpect (show x)) "TODO" row col)

    w = EP.unsafeIndex pos
  in
  if pos == end then
    toError row col errEof
  else if ord c < 32 && ord c /= 10 && ord c /= 13 then
    error $ "character no. " ++ show (ord c) ++  "can't handle cotrol characters except for line feed (LF) and carrige return (CR)"
  else if ord c == 127 then
    error "can't handle DEL character"
  else if ord c > 127 then
    error "can't handle unicode"
  else if fromIntegral w == ord c then
    stringHelp cs (updatePos w s) toOk toError
  else
    toError row col (errExpect c)


updatePos :: Word8 -> EP.State -> EP.State
updatePos w (EP.State src pos end indent row col) =
  let
    (row', col') =
      case w of
        0x0a {- "\n" -} -> (row + 1, 1)

        -- The doccumentation for `Text.Parsec.Char.updatePosChar` claims that
        -- carrige return ("\r") increments row by 1, just like newline ("\n").
        -- This doesn't appear to be the case from looking at the code:
        -- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/src/Text.Parsec.Pos.html#updatePosChar
        --
        -- Let's not devle into this unless it turns out that elm-format
        -- needs it.
        0x0d {- "\r" -} -> error "Can't handle carrige return"

        -- The parsec behaviour for tabs is to increment to the nearest
        -- 8'th collumn. Shoud we do this as well?
        -- Let's not implement this unless it turns out that elm-format
        -- needs it.
        0x09 {- "\t" -} -> error "Can't handle tabs"

        _ -> (row, col + 1)
  in
  EP.State src (plusPtr pos 1) end indent row' col'
