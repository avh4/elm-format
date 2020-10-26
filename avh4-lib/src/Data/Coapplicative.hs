module Data.Coapplicative (Coapplicative(..)) where

import Data.Functor.Compose
import Data.Functor.Identity


class Functor f => Coapplicative f where
    extract :: f a -> a

instance Coapplicative ((,) x) where
    extract (_, a) = a
    {-# INLINE extract #-}

instance Coapplicative Identity where
    extract = runIdentity
    {-# INLINE extract #-}

instance (Coapplicative a, Coapplicative b) => Coapplicative (Compose a b) where
    extract = extract . extract . getCompose
    {-# INLINE extract #-}
