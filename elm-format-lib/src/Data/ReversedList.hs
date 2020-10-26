module Data.ReversedList where

{-|
ReversedList can be used to avoid accidentally forgetting to reverse a list
(this most commonly occurs when implementing recursive algorithms that build up a
list that needs to be reversed in the termination case).
-}

newtype Reversed a = Reversed [a]


empty :: Reversed a
empty =
    Reversed []


push :: a -> Reversed a -> Reversed a
push a (Reversed list) =
    Reversed (a : list)


isEmpty :: Reversed a -> Bool
isEmpty (Reversed []) = True
isEmpty _ = False


toList :: Reversed a -> [a]
toList (Reversed list) =
    reverse list
