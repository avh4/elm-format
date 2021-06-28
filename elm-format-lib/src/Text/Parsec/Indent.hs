{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Indent
  ( runIndent
  , block
  , indented
  , checkIndent
  , withPos
  ) where

import Text.Parsec.Prim (Parser, updateParserState)
import qualified Parse.Primitives as EP


runIndent :: s -> a -> a
runIndent _ = id


block :: Parser a -> Parser [a]
block = undefined


indented :: Parser ()
indented = undefined


checkIndent :: Parser ()
checkIndent = undefined


withPos :: Parser a -> Parser a
withPos p =
  do  updateParserState (\(EP.State s p e _ r c sn nl) -> EP.State s p e c r c sn nl)
      p
