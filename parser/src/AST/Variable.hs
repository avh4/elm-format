module AST.Variable where

import AST.V0_16
import Data.Map.Strict


data Ref ns
    = VarRef ns LowercaseIdentifier
    | TagRef ns UppercaseIdentifier
    | OpRef SymbolIdentifier
    deriving (Eq, Ord, Show, Functor)


-- LISTINGS

-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a
    = ExplicitListing a Bool
    | OpenListing (Commented ())
    | ClosedListing
    deriving (Eq, Ord, Show)


type CommentedMap k v =
    Map k (Commented v)


-- | A value that can be imported or exported
data Value
    = Value !LowercaseIdentifier
    | OpValue SymbolIdentifier
    | Union (PostCommented UppercaseIdentifier) (Listing (CommentedMap UppercaseIdentifier ()))
    deriving (Eq, Ord, Show)
