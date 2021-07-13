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
import Text.Parsec.Prim (unexpected, Parser(..), (<|>), (<?>), try, many, skipMany)
import Text.Parsec.Error (Message(UnExpect), newErrorMessage)
import Text.Parsec.Char (anyChar)

import Control.Monad (mzero, liftM)


choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) mzero ps


many1 :: Parser a -> Parser [a]
many1 p =
  do  x <- p
      xs <- many p
      return (x:xs)


manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end =
  scan
  where
    scan =
      do{ _ <- end; return [] }
      <|>
      do{ x <- p; xs <- scan; return (x:xs) }


skipMany1 :: Parser a -> Parser ()
skipMany1 p =
  do  _ <- p
      skipMany p


option :: a -> Parser a -> Parser a
option x p = p <|> return x


optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = option Nothing (liftM Just p)


anyToken :: Parser Char
anyToken = anyChar


notFollowedBy :: Show a => Parser a -> Parser ()
notFollowedBy p =
  try $ do{ c <- try p; unexpected (show c) } <|> return ()


between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p =
  do{ _ <- open; x <- p; _ <- close; return x }


-- `eof` makes the parser fail if the entire inpute hasn't been consumed.
-- This function sits in an odd possition right now because the new parser
-- (`Parse.Primiteves.fromByteString` and `Parse.Primitives.fromSnippet`)
-- automatically does this whereas the adapter (`Text.Parsec.Prim.runParserT`)
-- does not.
--
-- I think the solution is to remove the eof behaviour from the new parser,
-- but we'll see
eof :: Parser ()
eof = notFollowedBy anyToken <?> "end of input"
