module Parse.Literal (literal) where

import Prelude hiding (exponent)
import Text.Parsec ((<|>), (<?>), digit, hexDigit, lookAhead, many1, option, string, try)
import Parse.Helpers (IParser, chr, str)

import AST.V0_16


literal :: IParser Literal
literal =
  num <|> ((\(s,b) -> Str s b) <$> str) <|> (Chr <$> chr)


num :: IParser Literal
num =
  toLiteral <$> (rawNumber <?> "a number")


toLiteral :: String -> Literal
toLiteral n
  | 'x' `elem` n         = IntNum (read n)
  | any (`elem` ".eE") n = FloatNum (read n)
  | otherwise            = IntNum (read n)


rawNumber :: IParser String
rawNumber =
  concat <$> sequence
    [ option "" minus
    , base16 <|> base10
    ]


base16 :: IParser String
base16 =
  do  try (string "0x")
      digits <- many1 hexDigit
      return ("0x" ++ digits)


base10 :: IParser String
base10 =
  concat <$> sequence
    [ many1 digit
    , option "" decimals
    , option "" exponent
    ]


minus :: IParser String
minus =
  try $ do
    string "-"
    lookAhead digit
    return "-"


decimals :: IParser String
decimals =
  do  try $ lookAhead (string "." >> digit)
      string "."
      n <- many1 digit
      return ('.' : n)


exponent :: IParser String
exponent =
  do  string "e" <|> string "E"
      op <- option "" (string "+" <|> string "-")
      n <- many1 digit
      return ('e' : op ++ n)
