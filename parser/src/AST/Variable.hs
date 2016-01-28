module AST.Variable where

import AST.V0_16


data Ref
    = VarRef String
    | OpRef String
    | WildcardRef
    deriving (Eq, Ord, Show)


-- LISTINGS

-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a = Listing
    { _explicits :: [Commented a]
    , _open :: Bool
    }
    deriving (Eq, Show)


openListing :: Listing a
openListing =
    Listing [] True


closedListing :: Listing a
closedListing =
    Listing [] False


listing :: [Commented a] -> Listing a
listing xs =
    Listing xs False


-- | A value that can be imported or exported
data Value
    = Value !Ref
    | Alias !String -- TODO: what is this?
    | Union (PostCommented String) (Listing String)
    deriving (Eq, Show)
