module Main exposing (..)

import ConstructorTagsAreDeduplicated exposing(A(X, X, X))
import ConstructorTagsAreMerged exposing (A(X, Y), B(A), B(..), A(C, D), C, C(..))
import ConstructorTagsAreSorted exposing (A(Y, Z, X, Yy, XYzZY))
import ExposedValuesAreDeduplicated exposing (x, (==/==), x, (==/==), Z, Z(..))
import ExposedValuesAreSorted exposing (x, (<>), a, (==/==), Y, Z(..))


x =
    ()
