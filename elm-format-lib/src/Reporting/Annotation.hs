{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Reporting.Annotation where

import Prelude ()
import Relude

import Data.Coapplicative
import Text.Show (showParen, showString, showsPrec)

import qualified Reporting.Region as R
import qualified Data.String as String


-- ANNOTATION

data Located a =
    A R.Region a
    deriving (Eq, Functor)


instance (Show a) => Show (Located a) where
    showsPrec p (A ann a) = showParen (p > 10) $
        showString $ String.unwords
            [ show ann
            , showsPrec 99 a ""
            ]

instance Coapplicative Located where
    extract (A _ x) = x
    {-# INLINE extract #-}

instance Foldable Located where
    foldMap f (A _ a) = f a

instance Traversable Located where
    traverse f (A region a) = fmap (A region) $ f a


-- CREATE

at :: R.Position -> R.Position -> a -> Located a
at start end value =
    A (R.Region start end) value


merge :: Located a -> Located b -> value -> Located value
merge (A region1 _) (A region2 _) value =
    A (R.merge region1 region2) value


sameAs :: Located a -> b -> Located b
sameAs (A annotation _) value =
    A annotation value
