{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.Parsec.Prim
  ( ParsecT
  , Stream(..)
  , (<?>)
  , (<|>)
  , lookAhead
  , try
  , many
  , skipMany
  , runParserT
  , getPosition
  , getInput
  , setInput
  , getState
  , updateState
  ) where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import qualified Data.Semigroup as Semigroup ( Semigroup(..) )
import qualified Data.Monoid as Monoid ( Monoid(..) )
import qualified Control.Monad.Fail as Fail

import Text.Parsec.Pos (SourceName, SourcePos)
import Text.Parsec.Error (ParseError)

data ParsecT s u m a
  = ParsecT {unParser :: forall b . () -> m b}

instance Functor (ParsecT s u m) where
    fmap f p = undefined

instance Applicative.Applicative (ParsecT s u m) where
    pure = undefined
    (<*>) = undefined


instance Applicative.Alternative (ParsecT s u m) where
    empty = undefined
    (<|>) = undefined

instance Monad (ParsecT s u m) where
    return = undefined
    p >>= f = undefined
    (>>) = undefined

instance Fail.MonadFail (ParsecT s u m) where
    fail = undefined

instance Semigroup.Semigroup a => Semigroup.Semigroup (ParsecT s u m a) where
    (<>)     = undefined

instance ( Monoid.Monoid a
         , Semigroup.Semigroup (ParsecT s u m a)
         ) => Monoid.Monoid (ParsecT s u m a) where
    mempty = undefined
    mappend = undefined
    mconcat = undefined

class (Monad m) => Stream s m t | s -> t where
    uncons :: s -> m (Maybe (t,s))

instance (Monad m) => Stream [tok] m tok where
    uncons []     = return $ Nothing
    uncons (t:ts) = return $ Just (t,ts)
    {-# INLINE uncons #-}

infixr 1 <|>
infix  0 <?>

(<|>) :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
(<|>) = undefined

(<?>) :: ParsecT s u m a -> String -> ParsecT s u m a
(<?>) = undefined

lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
lookAhead = undefined

try :: ParsecT s u m a -> ParsecT s u m a
try = undefined

many :: ParsecT s u m a -> ParsecT s u m [a]
many = undefined

skipMany ::ParsecT s u m a -> ParsecT s u m ()
skipMany = undefined

runParserT :: (Stream s m t) => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT = undefined

getPosition :: Monad m => ParsecT s u m SourcePos
getPosition = undefined

getInput :: Monad m => ParsecT s u m s
getInput = undefined

setPosition :: Monad m => SourcePos -> ParsecT s u m ()
setPosition = undefined

setInput :: Monad m => s -> ParsecT s u m ()
setInput = undefined

getState :: Monad m => ParsecT s u m u
getState = undefined

updateState :: Monad m => (u -> u) -> ParsecT s u m ()
updateState = undefined
