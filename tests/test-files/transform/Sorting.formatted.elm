module Main exposing (..)

import ConstructorTagsAreDeduplicated exposing (A(X))
import ConstructorTagsAreMerged exposing (A(C, D, X, Y), B(A), C(..))
import ConstructorTagsAreSorted exposing (A(X, XYzZY, Y, Yy, Z))
import ExposedValuesAreDeduplicated exposing ((==/==), Z(..), x)
import ExposedValuesAreSorted exposing ((<>), (==/==), Y, Z(..), a, x)


x =
    ()
