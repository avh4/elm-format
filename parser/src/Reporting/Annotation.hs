module Reporting.Annotation where

import Prelude hiding (map)
import qualified Reporting.PrettyPrint as P
import qualified Reporting.Region as R


-- ANNOTATION

data Located a =
    A R.Region a
    deriving (Show, Eq)


-- CREATE

at :: R.Position -> R.Position -> a -> Located a
at start end value =
    A (R.Region start end) value


atDontCare :: a -> Located a
atDontCare value =
    A (R.Region (R.Position 0 0) (R.Position 0 0 )) value


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


stripRegion :: Located a -> Located a
stripRegion (A _ value) =
    A (R.Region (R.Position 0 0) (R.Position 0 0)) value


-- PRETTY PRINT

instance (P.Pretty a) => P.Pretty (Located a) where
  pretty dealiaser parens (A _ value) =
      P.pretty dealiaser parens value
