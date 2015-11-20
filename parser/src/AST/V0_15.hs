{-# OPTIONS_GHC -Wall #-}

module AST.V0_15 where


data Comment
    = BlockComment String
    deriving (Eq, Show)


data Commented a =
    Commented [Comment] [Comment] a
    deriving (Eq, Show)
