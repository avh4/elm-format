{-# LANGUAGE DataKinds #-}
module AST.Listing where

import AST.V0_16
import Data.Map.Strict


-- | A listing of values. Something like (a,b,c) or (..) or (a,b,..)
data Listing a
    = ExplicitListing a Bool
    | OpenListing (C2 'BeforeTerm 'AfterTerm ())
    | ClosedListing
    deriving (Eq, Ord, Show, Functor) -- TODO: is Ord needed?

mergeListing :: (a -> a -> a) -> Listing a -> Listing a -> Listing a
mergeListing merge left right =
    case (left, right) of
        (ClosedListing, ClosedListing) -> ClosedListing
        (ClosedListing, OpenListing comments) -> OpenListing comments
        (OpenListing comments, ClosedListing) -> OpenListing comments
        (OpenListing (C (pre1, post1) ()), OpenListing (C (pre2, post2) ())) -> OpenListing (C (pre1 ++ pre2, post1 ++ post2) ())
        (ClosedListing, ExplicitListing a multiline) -> ExplicitListing a multiline
        (ExplicitListing a multiline, ClosedListing) -> ExplicitListing a multiline
        (OpenListing comments, ExplicitListing _a _multiline) -> OpenListing comments
        (ExplicitListing _a _multiline, OpenListing comments) -> OpenListing comments
        (ExplicitListing a multiline1, ExplicitListing b multiline2) -> ExplicitListing (merge a b) (multiline1 || multiline2)


type CommentedMap k v =
    Map k (C2 'BeforeTerm 'AfterTerm v)

mergeCommentedMap :: Ord k => (v -> v -> v) -> CommentedMap k v -> CommentedMap k v -> CommentedMap k v
mergeCommentedMap merge left right =
    let
        merge' (C (pre1, post1) a) (C (pre2, post2) b) =
            C (pre1 ++ pre2, post1 ++ post2) (merge a b)
    in
    unionWith merge' left right


-- | A value that can be imported or exported
data Value
    = Value !LowercaseIdentifier
    | OpValue SymbolIdentifier
    | Union (C1 'AfterTerm UppercaseIdentifier) (Listing (CommentedMap UppercaseIdentifier ()))
    deriving (Eq, Ord, Show) -- TODO: is Ord needed?
