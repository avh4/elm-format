{-# OPTIONS_GHC -Wall #-}
module AST.Helpers where

import qualified Data.Char                     as Char


isOp :: String -> Bool
isOp name = all isSymbol name


isSymbol :: Char -> Bool
isSymbol c = Char.isSymbol c || elem c "+-/*=.$<>:&|^?%#@~!"
