{-# LANGUAGE FlexibleInstances #-}
module Reporting.PrettyPrint where

import qualified Data.Map as Map

import qualified AST.Helpers as Help


-- PRETTY

class Pretty a where
  pretty :: Dealiaser -> Bool -> a -> Doc


type Dealiaser =
  Map.Map String String


-- HELPERS

commaCat :: [Doc] -> Doc
commaCat docs =
  cat (punctuate comma docs)


commaSep :: [Doc] -> Doc
commaSep docs =
  sep (punctuate comma docs)


parensIf :: Bool -> Doc -> Doc
parensIf bool doc =
  if bool then parens doc else doc


variable :: String -> Doc
variable x =
  if Help.isOp x then parens (text x) else text x
