module Parse.Literal (literal) where

import Prelude hiding (exponent)
import qualified Elm.String as ES
import Parse.ParsecAdapter
import Parse.IParser
import qualified Parse.String

import AST.V0_16


literal :: IParser LiteralValue
literal =
  num <|> (uncurry Str <$> str) <|> (Chr <$> chr)


num :: IParser LiteralValue
num =
  toLiteral <$> (rawNumber <?> "a number")


toLiteral :: String -> LiteralValue
toLiteral n
  | 'x' `elem` n         = IntNum (read n) HexadecimalInt
  | any (`elem` ("eE" :: String)) n = FloatNum (read n) ExponentFloat
  | any (`elem` ("." :: String)) n = FloatNum (read n) DecimalFloat
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


str :: IParser (String, StringRepresentation)
str =
  let
    toExpectation = newErrorUnknown "Expected a `\"`"

    toError e = newErrorUnknown ("Error parsing string: " ++ show e)
  in
  do  (s, representation) <- Parse.String.string toExpectation toError
      return (ES.toChars s, representation)


chr :: IParser Char
chr =
  let
    toExpecation = newErrorUnknown "Expected `'`"

    toError e = newErrorUnknown ("Error parsing char: " ++ show e)
  in
  do  s <- Parse.String.character toExpecation toError
      case ES.toChars s of
        [ c ] -> return c

        s ->
          error $ "A Char literal was parsed as containing " ++ show (length s) ++ " characters.  Please report this issue at https://github.com/avh4/elm-format/issues"
