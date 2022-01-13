{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Indexed where

import Data.Kind


-- Common typeclasses

type (~>) (f :: k -> Type) (g :: k -> Type) =
    forall (i :: k). f i -> g i


class HFunctor (f :: (k -> Type) -> k -> Type) where
   hmap :: (a ~> b) -> f a ~> f b


class HFoldable (t :: (k -> Type) -> k -> Type) where
    hFoldMap :: Monoid m => (forall i. f i -> m) -> t f a -> m


-- Recursion schemes

newtype Fix (f :: (k -> Type) -> k -> Type) (i :: k)
    = Fix { unFix :: f (Fix f) i}

deriving instance Show (f (Fix f) i) => Show (Fix f i)
deriving instance Eq (f (Fix f) i) => Eq (Fix f i)
deriving instance Ord (f (Fix f) i) => Ord (Fix f i)

fold :: HFunctor f => (f a ~> a) -> (Fix f ~> a)
fold f = f . hmap (fold f) . unFix

unfold :: HFunctor f => (a ~> f a) -> (a ~> Fix f)
unfold f = Fix . hmap (unfold f) . f


newtype Fix2 (ann :: Type -> Type) (f :: (k -> Type) -> k -> Type) (i :: k)
    = Fix2 { unFix2 :: ann (f (Fix2 ann f) i) }

deriving instance Show (ann (f (Fix2 ann f) i)) => Show (Fix2 ann f i)
deriving instance Eq (ann (f (Fix2 ann f) i)) => Eq (Fix2 ann f i)
deriving instance Ord (ann (f (Fix2 ann f) i)) => Ord (Fix2 ann f i)

fold2 ::
    HFunctor f =>
    Functor ann =>
    (forall i. ann (f a i) -> a i)
    -> (Fix2 ann f ~> a)
fold2 f = f . fmap (hmap $ fold2 f) . unFix2

unfold2 ::
    HFunctor f =>
    Functor ann =>
    (forall i. a i -> ann (f a i))
    -> (a ~> Fix2 ann f)
unfold2 f = Fix2 . fmap (hmap $ unfold2 f) . f

convert ::
    HFunctor f =>
    Functor ann1 =>
    (ann1 ~> ann2) ->
    (Fix2 ann1 f ~> Fix2 ann2 f)
convert f = fold2 (Fix2 . f)
