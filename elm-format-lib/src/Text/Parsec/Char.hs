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

import Text.Parsec.Prim (Parser)

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
string = undefined
