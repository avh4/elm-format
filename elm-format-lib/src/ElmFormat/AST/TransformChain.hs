{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}

module ElmFormat.AST.TransformChain (TransformChain, Carrier, map, mapMaybe, fold2,fold) where

import Prelude hiding (map)
import qualified Data.Indexed as I

{-| This module helps with combining multiple AST transformations into a single transformation that can be applied in a single pass.

Importantly, if any of the transformations in the chain result in a change,
then the entire chain will be applied to the new result, ensuring that the entire
output is fully processed.
-}


type TransformChain a = Carrier a -> Carrier a

newtype Carrier a =
    Carry { unCarry :: Either a a }


map :: Eq a => (a -> a) -> Carrier a -> Carrier a
map _ (Carry (Left a)) = Carry (Left a)
map f (Carry (Right a)) =
    Carry $
    case f a of
        new | new == a -> Right a
        new -> Left new


{-| Use this over `map` when possible, because returning `Nothing` means we don't have to do an equals check on the result.
-}
mapMaybe :: Eq a => (a -> Maybe a) -> Carrier a -> Carrier a
mapMaybe _ (Carry (Left a)) = Carry (Left a)
mapMaybe f (Carry (Right a)) =
    Carry $
    case f a of
        Nothing -> Right a
        Just new | new == a -> Right a
        Just new -> Left new


fold :: I.HFunctor f =>
    (forall j. Carrier (I.Fix f j) -> Carrier (I.Fix f j))
    -> I.Fix f i -> I.Fix f i
fold transform =
    I.foldTransform (unCarry . transform . Carry . Right . I.Fix)


fold2 :: I.HFunctor f => Functor ann =>
    (forall j. Carrier (I.Fix2 ann f j) -> Carrier (I.Fix2 ann f j))
    -> I.Fix2 ann f i -> I.Fix2 ann f i
fold2 transform =
    I.foldTransform2 (unCarry . transform . Carry . Right . I.Fix2)
