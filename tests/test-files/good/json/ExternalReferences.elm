module Target exposing (..)

import Date exposing (Month(..))
import Foo.Bar as Bar
import Foo.Baz exposing (..)
import Html
import Html.Attributes as Attr
import Html.Events as Event exposing (onClick)


token : String
token =
    "XYZZY"


theBestClass : Attr.Attribute msg
theBestClass =
    Attr.class token


text : String -> Html msg
text =
    Html.text


main : Html msg
main =
    Html.div
        [ theBestClass ]
        [ Html.text token ]
