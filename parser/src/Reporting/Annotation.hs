{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Reporting.Annotation where

import Prelude ()
import Relude

import Text.Show (showParen, showString, showsPrec)

import qualified Reporting.Region as R
import qualified Data.String as String


-- ANNOTATION

type Located a =
    Annotated R.Region a


data Annotated ann a =
    A ann a
    deriving (Eq)


instance Functor (Annotated ann) where
    fmap f (A region a) =
        A region (f a)


instance (Show a, Show ann) => Show (Annotated ann a) where
    showsPrec p (A ann a) = showParen (p > 10) $
        showString $ String.unwords
            [ show ann
            , showsPrec 99 a ""
            ]


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


-- MANIPULATE

map :: (a -> b) -> Located a -> Located b
map f (A annotation value) =
    A annotation (f value)


drop :: Located a -> a
drop (A _ value) =
    value
