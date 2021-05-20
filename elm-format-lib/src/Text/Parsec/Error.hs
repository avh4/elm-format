module Text.Parsec.Error
  ( Message
  , ParseError
  , errorPos
  , errorMessages
  ) where

import Text.Parsec.Pos (SourcePos)

data Message = Message
  deriving (Eq)

data ParseError = ParseError

instance Show ParseError where
  show = undefined

errorPos :: ParseError -> SourcePos
errorPos = undefined

errorMessages :: ParseError -> [Message]
errorMessages = undefined
