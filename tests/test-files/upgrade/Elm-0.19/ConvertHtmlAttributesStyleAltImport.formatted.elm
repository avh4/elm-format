module ConvertHtmlAttributesStyleAltImport exposing (convertStyle)

import Html
import Html.Attributes as HA


convertsStyle =
    Html.div [ HA.style "a" "1" ] []


doesntConvertLocalStyle f =
    Html.div [ style [ ( "a", "1" ) ] ] []


style x =
    x
