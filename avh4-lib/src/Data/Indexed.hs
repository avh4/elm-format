{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Indexed where


-- Common typeclasses

class IFunctor (f :: (k -> *) -> k -> *) where
   imap :: (forall i. a i -> b i) -> (forall i. f a i -> f b i)


class Foldable (t :: (k -> *) -> k -> *) where
    foldMap :: Monoid m => (forall i. f i -> m) -> t f a -> m


-- Recursion schemes

newtype Fix (ann :: * -> *) (f :: (k -> *) -> k -> *) (i :: k)
    = Fix { unFix :: ann (f (Fix ann f) i) }

deriving instance Show (ann (f (Fix ann f) i)) => Show (Fix ann f i)
deriving instance Eq (ann (f (Fix ann f) i)) => Eq (Fix ann f i)
deriving instance Ord (ann (f (Fix ann f) i)) => Ord (Fix ann f i)

cata ::
    Functor ann =>
    IFunctor f =>
    (forall i. ann (f a i) -> a i)
    -> (forall i. Fix ann f i -> a i)
cata f = f . fmap (imap $ cata f) . unFix


ana ::
    Functor ann =>
    IFunctor f =>
    (forall i. a i -> ann (f a i))
    -> (forall i. a i -> Fix ann f i)
ana f = Fix . fmap (imap $ ana f) . f


convert ::
  Functor ann1 =>
  IFunctor f =>
  (forall x. ann1 x -> ann2 x) ->
  (forall i. Fix ann1 f i -> Fix ann2 f i)
convert f = cata (Fix . f)
