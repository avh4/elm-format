module ElmFormat.KnownContents
    ( KnownContents
    , fromFunction
    , isKnown, get
    )
    where

import AST.V0_16

newtype KnownContents =
    KnownContents ([UppercaseIdentifier] -> Maybe [LocalName]) -- return Nothing if the contents are unknown

instance Semigroup KnownContents where
    (KnownContents a) <> (KnownContents b) = KnownContents (\ns -> a ns <> b ns)

instance Monoid KnownContents where
    mempty = fromFunction (const Nothing)


fromFunction :: ([UppercaseIdentifier] -> Maybe [LocalName]) -> KnownContents
{-# INLINE fromFunction #-}
fromFunction = KnownContents


isKnown :: KnownContents -> [UppercaseIdentifier] -> Bool
isKnown (KnownContents lookup) =
    maybe False (const True) . lookup


get :: [UppercaseIdentifier] -> KnownContents -> Maybe [LocalName]
{-# INLINE get #-}
get ns (KnownContents lookup) =
    lookup ns
