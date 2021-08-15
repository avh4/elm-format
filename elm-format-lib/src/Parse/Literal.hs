module Parse.Literal (literal) where

import Prelude hiding (exponent)
import qualified Elm.String as ES
import Parse.ParsecAdapter
import Parse.IParser
import qualified Parse.String
import qualified Parse.Number

import AST.V0_16


literal :: IParser LiteralValue
literal =
  num <|> (uncurry Str <$> str) <|> (Chr <$> chr)


num :: IParser LiteralValue
num =
  let
    toExpectation = parseError (Expect  "a digit")

    toError e = parseError (Message $ "Error parsing number: " ++ show e)
  in
  do  negate <- option False (return True <$> minus)
      n <- Parse.Number.number toExpectation toError
      case n of
        Parse.Number.Int int representation ->
          let num = if negate then -int else int in
          return $ IntNum num representation

        Parse.Number.Float float representation ->
          let num = if negate then -float else float in
          return $ FloatNum num representation


minus :: IParser String
minus =
  try $ do
    _ <- string "-"
    _ <- lookAhead digit
    return "-"


str :: IParser (String, StringRepresentation)
str =
  let
    toExpectation = parseError (Expect "\"`")

    toError e = parseError (Message $ "Error parsing string: " ++ show e)
  in
  do  (s, representation) <- Parse.String.string toExpectation toError
      return (ES.toChars s, representation)


chr :: IParser Char
chr =
  let
    toExpecation = parseError (Expect "'")

    toError e = parseError (Message $ "Error parsing char: " ++ show e)
  in
  do  s <- Parse.String.character toExpecation toError
      case ES.toChars s of
        [ c ] -> return c

        s ->
          error $ "A Char literal was parsed as containing " ++ show (length s) ++ " characters.  Please report this issue at https://github.com/avh4/elm-format/issues"
