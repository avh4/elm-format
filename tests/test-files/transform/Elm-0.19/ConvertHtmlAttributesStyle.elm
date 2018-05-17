import Html
import Html.Attributes exposing (style)

convertsStyle = Html.div [Html.Attributes.style [("a", "1")]] []
convertsExposedStyle = Html.div [style [("a", "1")]] []

doesntConvertOtherFunctions f = Html.div [ f [("a", "1")] ] []
