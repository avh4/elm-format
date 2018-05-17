import Html
import Html.Attributes exposing (style)

convertsStyle = Html.div [Html.Attributes.style [("a", "1")]] []
convertsExposedStyle = Html.div [style [("a", "1")]] []

doesntConvertOtherFunctions f = Html.div [ f [("a", "1")] ] []

comments = Html.div{-A-}[{-B-}style{-C-}[{-D-}({-E-}"a"{-F-},{-G-}"1"{-H-}){-I-},{-J-}({-K-}"b"{-L-},{-M-}"2"{-N-}){-O-}]{-P-}]{-Q-}[]
