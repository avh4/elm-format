module Parse.Literal (literal) where

import Prelude hiding (exponent)
import Text.Parsec ((<|>), (<?>), digit, hexDigit, lookAhead, many1, option, string, try, char, notFollowedBy, choice, anyChar, satisfy, manyTill)
import Parse.Helpers (processAs, escaped, expecting, sandwich, betwixt)
import Parse.IParser

import AST.V0_16

import qualified Text.Parsec.Token


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


str :: IParser (String, Bool)
str =
  expecting "a string" $
  do  (s, multi) <- choice [ multiStr, singleStr ]
      result <- processAs Text.Parsec.Token.stringLiteral . sandwich '\"' $ concat s
      return (result, multi)
  where
    rawString quote insides =
        quote >> manyTill insides quote

    multiStr  =
        do  result <- rawString (try (string "\"\"\"")) multilineStringChar
            return (result, True)
    singleStr =
        do  result <- rawString (char '"') stringChar
            return (result, False)

    stringChar :: IParser String
    stringChar = choice [ newlineChar, escaped '\"', (:[]) <$> satisfy (/= '\"') ]

    multilineStringChar :: IParser String
    multilineStringChar =
        do noEnd
           choice [ newlineChar, escaped '\"', expandQuote <$> anyChar ]
        where
          noEnd = notFollowedBy (string "\"\"\"")
          expandQuote c = if c == '\"' then "\\\"" else [c]

    newlineChar :: IParser String
    newlineChar =
        choice
            [ char '\n' >> return "\\n"
            , char '\r' >> choice
                [ char '\n' >> return "\\n"
                , return "\\r"
                ]
            ]


chr :: IParser Char
chr =
    betwixt '\'' '\'' character <?> "a character"
  where
    nonQuote = satisfy (/='\'')

    character =
      do  c <- choice
                [ escaped '\''
                , (:) <$> char '\\' <*> many1 nonQuote
                , (:[]) <$> nonQuote
                ]

          processAs Text.Parsec.Token.charLiteral $ sandwich '\'' c
