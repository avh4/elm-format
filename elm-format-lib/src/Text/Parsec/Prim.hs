{-# LANGUAGE PolymorphicComponents #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.Parsec.Prim
  ( Parser
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

import Parse.State (State)

data Parser a
  = Parser

instance Functor Parser where
    fmap f p = undefined

instance Applicative.Applicative Parser where
    pure = undefined
    (<*>) = undefined


instance Applicative.Alternative Parser where
    empty = undefined
    (<|>) = undefined

instance Monad Parser where
    return = undefined
    p >>= f = undefined
    (>>) = undefined

instance Fail.MonadFail (Parser) where
    fail = undefined

infixr 1 <|>
infix  0 <?>

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = undefined

(<?>) :: Parser a -> String -> Parser a
(<?>) = undefined

lookAhead :: Parser a -> Parser a
lookAhead = undefined

try :: Parser a -> Parser a
try = undefined

many :: Parser a -> Parser [a]
many = undefined

skipMany ::Parser a -> Parser ()
skipMany = undefined

runParserT :: Parser a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT = undefined

getPosition :: Parser SourcePos
getPosition = undefined

getInput :: Parser s
getInput = undefined

setInput :: s -> Parser ()
setInput = undefined

getState :: Parser State
getState = undefined

updateState :: (State -> State) -> Parser ()
updateState = undefined
