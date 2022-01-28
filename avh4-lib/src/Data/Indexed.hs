{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Indexed where

import Data.Kind
import Control.Monad.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import Data.Functor.Const (Const(..))


-- Common typeclasses

type (~>) (f :: k -> Type) (g :: k -> Type) =
    forall (i :: k). f i -> g i


class HFunctor (f :: (k -> Type) -> k -> Type) where
   hmap :: (a ~> b) -> f a ~> f b


class HFoldable (t :: (k -> Type) -> k -> Type) where
    hFoldMap :: Monoid m => (forall i. f i -> m) -> t f a -> m
    hFold :: Monoid m => t (Const m) a -> m
    hFold = hFoldMap getConst


-- Recursion schemes

newtype Fix (f :: (k -> Type) -> k -> Type) (i :: k)
    = Fix { unFix :: f (Fix f) i}

deriving instance Show (f (Fix f) i) => Show (Fix f i)
deriving instance Eq (f (Fix f) i) => Eq (Fix f i)
deriving instance Ord (f (Fix f) i) => Ord (Fix f i)

fold :: HFunctor f => (f a ~> a) -> (Fix f ~> a)
fold f = f . hmap (fold f) . unFix

foldTransform :: HFunctor f => (forall i. f a i -> Either (Fix f i) (a i)) -> (Fix f ~> a)
foldTransform f = either (foldTransform f) id . f . hmap (foldTransform f) . unFix

foldMaybeTransform :: HFunctor f => (forall i. f (Fix f) i -> Maybe (Fix f i)) -> (Fix f ~> Fix f)
foldMaybeTransform f orig = fromMaybe orig $ f $ hmap (foldMaybeTransform f) $ unFix orig


unfold :: HFunctor f => (a ~> f a) -> (a ~> Fix f)
unfold f = Fix . hmap (unfold f) . f


newtype Fix2 (ann :: Type -> Type) (f :: (k -> Type) -> k -> Type) (i :: k)
    = Fix2 { unFix2 :: ann (f (Fix2 ann f) i) }

deriving instance Show (ann (f (Fix2 ann f) i)) => Show (Fix2 ann f i)
deriving instance Eq (ann (f (Fix2 ann f) i)) => Eq (Fix2 ann f i)
deriving instance Ord (ann (f (Fix2 ann f) i)) => Ord (Fix2 ann f i)

fold2 ::
    HFunctor f => Functor ann =>
    (forall i. ann (f a i) -> a i)
    -> (Fix2 ann f ~> a)
fold2 f = f . fmap (hmap $ fold2 f) . unFix2

foldConst2 ::
    HFunctor f => Functor ann =>
    (forall i. ann (f (Const a) i) -> a)
    -> (forall i. Fix2 ann f i -> a)
foldConst2 f = getConst . fold2 (Const . f)

foldTransform2 ::
    HFunctor f => Functor ann =>
    (forall i. ann (f a i) -> Either (Fix2 ann f i) (a i))
    -> (Fix2 ann f ~> a)
foldTransform2 f =
    either (foldTransform2 f) id . f . fmap (hmap $ foldTransform2 f) . unFix2

foldMaybeTransform2 ::
    HFunctor f => Functor ann =>
    (forall i. ann (f (Fix2 ann f) i) -> Maybe (Fix2 ann f i))
    -> (Fix2 ann f ~> Fix2 ann f)
foldMaybeTransform2 f orig =
    fromMaybe orig $ f $ hmap (foldMaybeTransform2 f) <$> unFix2 orig


unfold2 ::
    HFunctor f => Functor ann =>
    (forall i. a i -> ann (f a i))
    -> (a ~> Fix2 ann f)
unfold2 f = Fix2 . fmap (hmap $ unfold2 f) . f

convert ::
    HFunctor f =>
    Functor ann1 =>
    (ann1 ~> ann2) ->
    (Fix2 ann1 f ~> Fix2 ann2 f)
convert f = fold2 (Fix2 . f)

{-| Convenience function for applying a function that works with `Fix2` to a `Fix`. -}
fold2Identity ::
    HFunctor f => HFunctor g =>
    (forall i. f (Fix2 Identity g) i -> Identity (g (Fix2 Identity g) i))
    -> (Fix f ~> Fix g)
fold2Identity f =
    fold2 (Fix . runIdentity)
    . fold2 (Fix2 . f . runIdentity)
    . fold (Fix2 . Identity)
