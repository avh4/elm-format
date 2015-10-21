{-# OPTIONS_GHC -Wall #-}

module AST.V0_15 where


data Comment
    = BlockComment String
    deriving (Show)


data Commented a =
    Commented [Comment] a
    deriving (Show)
