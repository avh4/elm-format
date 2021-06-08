{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Indent
  ( runIndent
  , block
  , indented
  , checkIndent
  , withPos
  ) where

import Text.Parsec.Prim (Parser)


runIndent :: s -> a -> a
runIndent _ = id


block :: Parser a -> Parser [a]
block = undefined


indented :: Parser ()
indented = undefined


checkIndent :: Parser ()
checkIndent = undefined


withPos :: Parser a -> Parser a
withPos = undefined
