{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Fix where

newtype Fix f = Fix { unFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f) 


cata ::
    Functor f =>
    (f a -> a)
    -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix
