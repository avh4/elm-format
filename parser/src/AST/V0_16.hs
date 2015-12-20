{-# OPTIONS_GHC -Wall #-}

module AST.V0_16 where


data Comment
    = BlockComment [String]
    | LineComment String
    deriving (Eq, Show)


data Commented a =
    Commented [Comment] [Comment] a
    deriving (Eq, Show)


data Literal
    = IntNum Int
    | FloatNum Double
    | Chr Char
    | Str String Bool
    | Boolean Bool
    deriving (Eq, Ord, Show)
