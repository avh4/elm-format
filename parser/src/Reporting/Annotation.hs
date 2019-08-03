module Reporting.Annotation where

import Prelude hiding (map)
import qualified Reporting.Region as R
import qualified Data.String as String


-- ANNOTATION

data Located a =
    A R.Region a
    deriving (Eq)


instance Functor Located where
    fmap f (A region a) =
        A region (f a)


instance (Show a) => Show (Located a) where
    showsPrec p (A r a) = showParen (p > 10) $
        showString $ String.unwords
            [ "at"
            , show (R.line $ R.start r)
            , show (R.column $ R.start r)
            , show (R.line $ R.end r)
            , show (R.column $ R.end r)
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


class Strippable a where
  stripRegion :: a -> a


instance Strippable (Located a) where
  stripRegion (A _ value) =
    A (R.Region (R.Position 0 0) (R.Position 0 0)) value
