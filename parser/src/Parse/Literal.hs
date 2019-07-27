module Parse.Literal (literal) where

import Prelude hiding (exponent)
import Data.Char (digitToInt, isSpace)
import Text.Parsec ((<|>), (<?>), digit, hexDigit, lookAhead, many1, option, string, try, char, notFollowedBy, choice, anyChar, satisfy, manyTill, many, between, skipMany, skipMany1)
import Text.Parsec.Char (octDigit, space, upper)
import Parse.Helpers (processAs, escaped, expecting, sandwich, betwixt)
import Parse.IParser

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


str :: IParser (String, Bool)
str =
  expecting "a string" $
  do  (s, multi) <- choice [ multiStr, singleStr ]
      result <- processAs stringLiteral . sandwich '\"' $ concat s
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

          processAs charLiteral $ sandwich '\'' c


--
-- Stuff forked from Text.Parsec.Token
--

charLiteral :: IParser Char
charLiteral     = lexeme (between (char '\'')
                                  (char '\'' <?> "end of character")
                                  characterChar )
                <?> "character"

characterChar :: IParser Char
characterChar   = charLetter <|> charEscape
                <?> "literal character"

charEscape :: IParser Char
charEscape      = do{ _ <- char '\\'; escapeCode }
charLetter :: IParser Char
charLetter      = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026') || (c == '\t'))


stringLiteral :: IParser String
stringLiteral   = lexeme (
                  do{ str <- between (char '"')
                                      (char '"' <?> "end of string")
                                      (many stringChar)
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "literal string")

stringChar :: IParser (Maybe Char)
stringChar      =   do{ c <- stringLetter; return (Just c) }
                <|> stringEscape
                <?> "string character"

stringLetter :: IParser Char
stringLetter    = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026') || (c == '\t'))

stringEscape :: IParser (Maybe Char)
stringEscape    = do{ _ <- char '\\'
                    ;     do{ _ <- escapeGap  ; return Nothing }
                      <|> do{ _ <- escapeEmpty; return Nothing }
                      <|> do{ esc <- escapeCode; return (Just esc) }
                    }

escapeEmpty :: IParser Char
escapeEmpty     = char '&'
escapeGap :: IParser Char
escapeGap       = do{ _ <- many1 space
                    ; char '\\' <?> "end of string gap"
                    }



-- escape codes
escapeCode :: IParser Char
escapeCode      = charEsc <|> charNum <|> charAscii <|> charControl
                <?> "escape code"

charControl :: IParser Char
charControl     = do{ _ <- char '^'
                    ; code <- upper
                    ; return (toEnum (fromEnum code - fromEnum 'A' + 1))
                    }

charNum :: IParser Char
charNum         = do{ code <- decimal
                              <|> do{ _ <- char 'o'; number 8 octDigit }
                              <|> do{ _ <- char 'x'; number 16 hexDigit }
                              <|> do{ _ <- char 'u'; between (char '{') (char '}') (number 16 hexDigit) }
                    ; if code > 0x10FFFF
                      then fail "invalid escape sequence"
                      else return (toEnum (fromInteger code))
                    }

charEsc :: IParser Char
charEsc         = choice (map parseEsc escMap)
                where
                  parseEsc :: (Char, a) -> IParser a
                  parseEsc (c,code)     = do{ _ <- char c; return code }

charAscii :: IParser Char
charAscii       = choice (map parseAscii asciiMap)
                where
                  parseAscii :: (String, a) -> IParser a
                  parseAscii (asc,code) = try (do{ _ <- string asc; return code })


-- escape code tables
escMap :: [(Char, Char)]
escMap          = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")
asciiMap :: [(String, Char)]
asciiMap        = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes     = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                    "FS","GS","RS","US","SP"]
ascii3codes :: [String]
ascii3codes     = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                    "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                    "CAN","SUB","ESC","DEL"]

ascii2 :: [Char]
ascii2          = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
                    '\EM','\FS','\GS','\RS','\US','\SP']
ascii3 :: [Char]
ascii3          = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
                    '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
                    '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']


decimal :: IParser Integer
decimal         = number 10 digit

number :: Integer -> IParser Char -> IParser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }



lexeme :: IParser a -> IParser a
lexeme p
    = do{ x <- p; whiteSpace; return x  }


--whiteSpace
whiteSpace :: IParser ()
whiteSpace = skipMany (simpleSpace <?> "")

simpleSpace :: IParser ()
simpleSpace =
    skipMany1 (satisfy isSpace)
