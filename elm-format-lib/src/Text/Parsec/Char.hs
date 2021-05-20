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

import Text.Parsec.Prim (ParsecT, Stream)

oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
oneOf = undefined

space :: (Stream s m Char) => ParsecT s u m Char
space = undefined

upper :: (Stream s m Char) => ParsecT s u m Char
upper = undefined

lower :: (Stream s m Char) => ParsecT s u m Char
lower = undefined

alphaNum :: (Stream s m Char => ParsecT s u m Char)
alphaNum = undefined

letter :: Stream s m Char => ParsecT s u m Char
letter = undefined

digit :: (Stream s m Char) => ParsecT s u m Char
digit = undefined

hexDigit :: (Stream s m Char) => ParsecT s u m Char
hexDigit = undefined

octDigit :: (Stream s m Char) => ParsecT s u m Char
octDigit = undefined

char :: (Stream s m Char) => Char -> ParsecT s u m Char
char = undefined

anyChar :: (Stream s m Char) => ParsecT s u m Char
anyChar             = satisfy (const True)

satisfy :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
satisfy = undefined

string :: (Stream s m Char) => String -> ParsecT s u m String
string = undefined
