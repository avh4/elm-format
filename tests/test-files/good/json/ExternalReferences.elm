module Target exposing (..)

import Date exposing (Month(..))
import Foo.Bar as Bar
import Foo.Baz exposing (..)
import Html
import Html.Attributes
import Html.Events as Event exposing (onClick)


token : String
token =
    "XYZZY"


text : String -> Html msg
text =
    Html.text


main : Html msg
main =
    Html.text token
