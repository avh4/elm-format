module Sorting exposing (x)

import ConstructorTagsAreDeduplicated exposing(A(X, X, X))
import ConstructorTagsAreMerged exposing (A(X, Y), B(A), B(..), A(C, D), C, C(..))
import ConstructorTagsAreSorted exposing (A(Y, Z, X, Yy, XYzZY))
import ExposedValuesAreDeduplicated exposing (x, (==/==), x, (==/==), Z, Z(..))
import ExposedValuesAreSorted exposing (x, (<>), a, (==/==), Y, Z(..))
import ImportsAreMerged exposing (A(X), b)
import ImportsAreMerged exposing (A(Y), z)
import ImportsAreMerged2
import ImportsAreMerged2 exposing (..)
import ImportsAreMerged3 exposing (x)
import ImportsAreMerged3 exposing (..)
import ImportsWithConflictingAs as Overridden
import ImportsWithConflictingAs as Correct
import ImportsWithConflictingAs2
import ImportsWithConflictingAs2 as Correct


x =
    ()
