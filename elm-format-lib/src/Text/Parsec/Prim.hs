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
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail

import Text.Parsec.Pos (SourceName, SourcePos, newPos)
import Text.Parsec.Error (ParseError)

import qualified Parse.Primitives as EP
import Parse.State (State)

import qualified Text.Parsec.Error as Error


unknownError :: EP.Row -> EP.Col -> ParseError
unknownError row col =
  Error.newErrorUnknown $ newPos "" row col


data Parser a
  = Parser (EP.Parser ParseError a)


instance Functor Parser where
    fmap f (Parser p) = Parser (fmap f p)


instance Applicative.Applicative Parser where
    pure = return
    (Parser f) <*> (Parser a) = Parser (f <*> a)

instance Applicative.Alternative Parser where
    empty = mzero
    (<|>) = mplus


instance Monad Parser where
    return = Parser . return
    (Parser p) >>= f = Parser $ p >>= unwrap . f

unwrap :: Parser a -> EP.Parser ParseError a
unwrap (Parser p) = p


instance Fail.MonadFail Parser where
    fail = undefined


instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus


parserZero :: Parser a
parserZero =
  Parser $ EP.Parser $ \state _ _ _ eerr ->
    let
      (EP.State _ _ _ _ row col) = state
    in
    eerr row col unknownError


parserPlus :: Parser a -> Parser a -> Parser a
parserPlus (Parser p) (Parser q) =
  Parser $ EP.oneOf (\row col -> undefined) [p, q]


infixr 1 <|>
infix  0 <?>


(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = mplus


(<?>) :: Parser a -> String -> Parser a
(<?>) = undefined


lookAhead :: Parser a -> Parser a
lookAhead = undefined


try :: Parser a -> Parser a
try (Parser (EP.Parser parser)) =
  Parser $ EP.Parser $ \s cok eok _ err ->
    parser s cok eok err err

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
