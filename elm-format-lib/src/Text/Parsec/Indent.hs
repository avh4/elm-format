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
import Text.Parsec.Prim (ParsecT, Stream)

import Control.Monad.State (State)

type IndentParser s u a = ParsecT s u (State SourcePos) a

runIndent :: SourceName -> State SourcePos a -> a
runIndent = undefined

block :: Stream s (State SourcePos) z => IndentParser s u a -> IndentParser s u [a]
block = undefined

indented :: Stream s (State SourcePos) z => IndentParser s u ()
indented = undefined

checkIndent :: Stream s (State SourcePos) z => IndentParser s u ()
checkIndent = undefined

withPos :: Stream s (State SourcePos) z => IndentParser s u a -> IndentParser s u a
withPos = undefined
