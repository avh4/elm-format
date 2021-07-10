{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Indent
  ( runIndent
  , block
  , indented
  , checkIndent
  , withPos
  ) where

import Text.Parsec.Prim (Parser, getParserState, updateParserState)
import Text.Parsec.Combinator (many1)
import qualified Parse.Primitives as EP

import Data.Word (Word16)


runIndent :: s -> a -> a
runIndent _ = id


block :: Parser a -> Parser [a]
block p = withPos $ do
    r <- many1 (checkIndent >> p)
    return r


indented :: Parser ()
indented =
  do  (EP.State _ _ _ indent _ col _ _) <- getParserState
      if col <= indent then fail "not indented" else do return ()


checkIndent :: Parser ()
checkIndent =
  do  (EP.State _ _ _ indent _ col _ _) <- getParserState
      if indent == col then return () else fail "indentation doesn't match"


withPos :: Parser a -> Parser a
withPos (EP.Parser p) =
  EP.Parser $ \s@(EP.State _ _ _ indent _ col _ _) cok eok cerr eerr ->
    let
      cok' x s' = cok x (setIndent indent s')
      eok' x s' = eok x (setIndent indent s')
    in
    p (setIndent col s) cok' eok' cerr eerr


setIndent :: Word16 -> EP.State -> EP.State
setIndent indent (EP.State s p e _ r c nl sn) =
  EP.State s p e indent r c nl sn
