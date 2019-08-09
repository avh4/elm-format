module Sorting exposing (x)

import ConstructorTagsAreDeduplicated exposing (A(X))
import ConstructorTagsAreMerged exposing (A(C, D, X, Y), B(..), C(..))
import ConstructorTagsAreSorted exposing (A(X, XYzZY, Y, Yy, Z))
import ExposedValuesAreDeduplicated exposing ((==/==), Z(..), x)
import ExposedValuesAreSorted exposing ((<>), (==/==), Y, Z(..), a, x)
import ImportsAreMerged exposing (A(X, Y), b, z)
import ImportsAreMerged2 exposing (..)
import ImportsAreMerged3 exposing (..)
import ImportsWithConflictingAs as Correct
import ImportsWithConflictingAs2 as Correct


x =
    ()
