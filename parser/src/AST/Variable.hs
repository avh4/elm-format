module AST.Variable where


data Ref
    = VarRef String
    | OpRef String
    | WildcardRef
    deriving (Eq, Ord, Show)


-- LISTINGS

-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a = Listing
    { _explicits :: [a]
    , _open :: Bool
    }
    deriving (Eq, Ord, Show)


instance Functor Listing where
    fmap fn (Listing explicits open) =
        Listing (map fn explicits) open


openListing :: Listing a
openListing =
    Listing [] True


closedListing :: Listing a
closedListing =
    Listing [] False


listing :: [a] -> Listing a
listing xs =
    Listing xs False


-- | A value that can be imported or exported
data Value
    = Value !Ref
    | Alias !String
    | Union !String !(Listing String)
    deriving (Eq, Show)
