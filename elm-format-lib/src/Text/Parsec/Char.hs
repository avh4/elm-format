{-# LANGUAGE CPP, FlexibleContexts #-}

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

import qualified Parse.Primitives as EP
import Text.Parsec.Prim (Parser(..))
import Text.Parsec.Pos (newPos)
import Text.Parsec.Error (Message(SysUnExpect, Expect), newErrorMessage, setErrorMessage)

import Foreign.Ptr (plusPtr)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Word (Word8)
import Data.Char (chr, ord)


oneOf :: [Char] -> Parser Char
oneOf = undefined

space :: Parser Char
space = undefined

upper :: Parser Char
upper = undefined

lower :: Parser Char
lower = undefined

alphaNum :: Parser Char
alphaNum = undefined

letter :: Parser Char
letter = undefined

digit :: Parser Char
digit = undefined

hexDigit :: Parser Char
hexDigit = undefined

octDigit :: Parser Char
octDigit = undefined

char :: Char -> Parser Char
char = undefined

anyChar :: Parser Char
anyChar             = satisfy (const True)

satisfy :: (Char -> Bool) -> Parser Char
satisfy = undefined


string :: String -> Parser String
string = fmap decode . word8s . concatMap encodeChar


word8s :: [Word8] -> Parser [Word8]
word8s [] = return []
word8s (w:ws) =
  Parser $ EP.Parser $ \s@(EP.State _ pos end _ row col) cok _ cerr eerr ->
    let
      sourcePos = newPos "TODO - SourceName" row col

      errEof _ _ = setErrorMessage (Expect (show (w:ws)))
                    (newErrorMessage (SysUnExpect "") sourcePos)

      errExpect x _ _ = setErrorMessage (Expect (show (w:ws)))
                          (newErrorMessage (SysUnExpect (show x)) sourcePos)

      walk [] s' = cok (w:ws) s'
      walk (w':ws') s'@(EP.State _ pos' _ _ row' col') =
        if pos' == end then
          cerr row col errEof
        else if EP.unsafeIndex pos' == w' then
          walk ws' (updatePos w' s')
        else
          cerr row col (errExpect w')
    in
    if pos == end then
      eerr row col errEof
    else if EP.unsafeIndex pos == w then
        walk ws (updatePos w s)
    else
      eerr row col (errExpect w)


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


-- https://hackage.haskell.org/package/utf8-string-1.0.2/docs/src/Codec.Binary.UTF8.String.html#decode
decode :: [Word8] -> String
decode [    ] = ""
decode (c:cs)
  | c < 0x80  = chr (fromEnum c) : decode cs
  | c < 0xc0  = replacement_character : decode cs
  | c < 0xe0  = multi1
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacement_character : decode cs
  where
    multi1 = case cs of
      c1 : ds | c1 .&. 0xc0 == 0x80 ->
        let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
        in if d >= 0x000080 then toEnum d : decode ds
                            else replacement_character : decode ds
      _ -> replacement_character : decode cs

    multi_byte :: Int -> Word8 -> Int -> [Char]
    multi_byte i mask overlong = aux i cs (fromEnum (c .&. mask))
      where
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
          | otherwise = replacement_character : decode rs

        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)

        aux _ rs     _ = replacement_character : decode rs


replacement_character :: Char
replacement_character = '\xfffd'
