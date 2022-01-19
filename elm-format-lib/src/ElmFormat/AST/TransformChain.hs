{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}

module ElmFormat.AST.TransformChain where
import qualified Data.Indexed as I

{-| This module helps with combining multiple AST transformations into a single transformation that can be applied in a single pass.

Importantly, if any of the transformations in the chain result in a change,
then the entire chain will be applied to the new result, ensuring that the entire
output is fully processed.
-}


newtype TransformChain a =
    TransformChain { unChain :: Either a a }


map :: Eq a => (a -> a) -> TransformChain a -> TransformChain a
map _ (TransformChain (Left a)) = TransformChain (Left a)
map f (TransformChain (Right a)) =
    TransformChain $
    case f a of
        new | new == a -> Right a
        new -> Left new


{-| Use this over `map` when possible, because returning `Nothing` means we don't have to do an equals check on the result.
-}
mapMaybe :: Eq a => (a -> Maybe a) -> TransformChain a -> TransformChain a
mapMaybe _ (TransformChain (Left a)) = TransformChain (Left a)
mapMaybe f (TransformChain (Right a)) =
    TransformChain $
    case f a of
        Nothing -> Right a
        Just new | new == a -> Right a
        Just new -> Left new


fold2 :: I.HFunctor f => Functor ann =>
    (forall j. TransformChain (I.Fix2 ann f j) -> TransformChain (I.Fix2 ann f j))
    -> I.Fix2 ann f i -> I.Fix2 ann f i
fold2 transform =
    I.foldTransform2 (unChain . transform . TransformChain . Right . I.Fix2)
