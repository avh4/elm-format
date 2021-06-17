module Text.Parsec.Combinator
  ( choice
  , many1
  , manyTill
  , skipMany1
  , option
  , optionMaybe
  , anyToken
  , notFollowedBy
  , between
  , eof
  ) where

import qualified Parse.Primitives as EP
import Text.Parsec.Prim (unexpected, Parser(..), (<|>), try, many)
import Text.Parsec.Error (Message(UnExpect), newErrorMessage)


choice :: [Parser a] -> Parser a
choice = undefined

many1 :: Parser a -> Parser [a]
many1 p =
  do  x <- p
      xs <- many p
      return (x:xs)

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill = undefined

skipMany1 :: Parser a -> Parser ()
skipMany1 = undefined

option :: a -> Parser a -> Parser a
option x p = p <|> return x

optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe = undefined

anyToken :: Parser Char
anyToken = undefined


notFollowedBy :: Show a => Parser a -> Parser ()
notFollowedBy p =
  try $ do{ c <- try p; unexpected (show c) } <|> return ()


between :: Parser open -> Parser close -> Parser a -> Parser a
between = undefined

eof :: Parser ()
eof = return ()
