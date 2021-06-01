{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Indent
  ( IndentParser
  , runIndent
  , block
  , indented
  , checkIndent
  , withPos
  ) where

import Text.Parsec.Pos (SourcePos, SourceName)
import Text.Parsec.Prim (Parser)

import Control.Monad.State (State)

type IndentParser a = Parser a

runIndent :: SourceName -> State SourcePos a -> a
runIndent = undefined

block :: IndentParser a -> IndentParser [a]
block = undefined

indented :: IndentParser ()
indented = undefined

checkIndent :: IndentParser ()
checkIndent = undefined

withPos :: IndentParser a -> IndentParser a
withPos = undefined
