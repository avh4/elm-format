{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Indent
  ( runIndent
  , block
  , indented
  , checkIndent
  , withPos
  ) where

import Text.Parsec.Prim (Parser, getParserState, updateParserState)
import qualified Parse.Primitives as EP


runIndent :: s -> a -> a
runIndent _ = id


block :: Parser a -> Parser [a]
block = undefined


indented :: Parser ()
indented =
  do  (EP.State _ _ _ indent _ col _ _) <- getParserState
      if col <= indent then fail "not indented" else do return ()


checkIndent :: Parser ()
checkIndent =
  do  (EP.State _ _ _ indent _ col _ _) <- getParserState
      if indent == col then return () else fail "indentation doesn't match"


withPos :: Parser a -> Parser a
withPos p =
  do  updateParserState (\(EP.State s p e _ r c sn nl) -> EP.State s p e c r c sn nl)
      p
