module Parse.Literal (literal) where

import Prelude hiding (exponent)
import Text.Parsec ((<|>), (<?>), digit, hexDigit, lookAhead, many1, option, string, try)
import Parse.Helpers (IParser, chr, str)

import AST.V0_16


literal :: IParser Literal
literal =
  num <|> (uncurry Str <$> str) <|> (Chr <$> chr)


num :: IParser Literal
num =
  toLiteral <$> (rawNumber <?> "a number")


toLiteral :: String -> Literal
toLiteral n
  | 'x' `elem` n         = IntNum (read n) HexadecimalInt
  | any (`elem` "eE") n  = FloatNum (read n) ExponentFloat
  | any (`elem` ".") n   = FloatNum (read n) DecimalFloat
  | otherwise            = IntNum (read n) DecimalInt


rawNumber :: IParser String
rawNumber =
  concat <$> sequence
    [ option "" minus
    , base16 <|> base10
    ]


base16 :: IParser String
base16 =
  do  _ <- try (string "0x")
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
    _ <- string "-"
    _ <- lookAhead digit
    return "-"


decimals :: IParser String
decimals =
  do  _ <- try $ lookAhead (string "." >> digit)
      _ <- string "."
      n <- many1 digit
      return ('.' : n)


exponent :: IParser String
exponent =
  do  _ <- string "e" <|> string "E"
      op <- option "" (string "+" <|> string "-")
      n <- many1 digit
      return ('e' : op ++ n)
