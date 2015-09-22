{-# OPTIONS_GHC -Wall #-}

module AST.V0_15 where


data Commented a =
    Commented [String] a
    deriving (Show)
