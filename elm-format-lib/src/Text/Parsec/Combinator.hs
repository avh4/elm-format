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

import Text.Parsec.Prim (ParsecT, Stream)

choice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choice = undefined

many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
many1 = undefined

manyTill :: Stream s m t => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill = undefined

skipMany1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
skipMany1 = undefined

option :: Stream s m t => a -> ParsecT s u m a -> ParsecT s u m a
option = undefined

optionMaybe :: Stream s m t => ParsecT s u m a -> ParsecT s u m (Maybe a)
optionMaybe = undefined

anyToken :: (Stream s m t, Show t) => ParsecT s u m t
anyToken = undefined

notFollowedBy :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()
notFollowedBy = undefined

between :: Stream s m t => ParsecT s u m open -> ParsecT s u m close -> ParsecT s u m a -> ParsecT s u m a
between = undefined

eof :: (Stream s m t, Show t) => ParsecT s u m ()
eof = undefined
