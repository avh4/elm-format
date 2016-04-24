module Css (Style, stylesheet, prettyPrint, ($), (#), (.), (@), (|$), (>$), ($=), (~), (!), html, body, header, nav, div, span, img, nowrap, button, h1, h2, h3, h4, p, ol, input, verticalAlign, display, opacity, width, minWidth, height, minHeight, padding, paddingTop, paddingBottom, paddingRight, paddingLeft, margin, marginTop, marginBottom, marginRight, marginLeft, boxSizing, overflowX, overflowY, whiteSpace, backgroundColor, color, media, textShadow, outline, solid, transparent, rgb, rgba, hex, pct, em, px, borderBox, visible, block, inlineBlock, inline, none, auto, inherit, noWrap, top, middle, bottom) where
{-| Functions for building stylesheets.

# Style
@docs Style, stylesheet, prettyPrint

# Statements
@docs ($), (#), (.), (@), ($=)

# Combinators
@docs (|$), (>$)

# Elements
@docs html, body, header, nav, div, span, img, nowrap, button, h1, h2, h3, h4, p, ol, input

# Attributes
@docs verticalAlign, display, opacity, width, minWidth, height, minHeight, padding, paddingTop, paddingBottom, paddingRight, paddingLeft, margin, marginTop, marginBottom, marginRight, marginLeft, boxSizing, overflowX, overflowY, whiteSpace, backgroundColor, color, media, textShadow, outline

# Values
@docs (~), (!), solid, transparent, rgb, rgba, hex, pct, em, px, borderBox, visible, block, inlineBlock, inline, none, auto, inherit, noWrap, top, middle, bottom
-}


import Css.Declaration as Declaration exposing (..)
import Css.Declaration.Output exposing (prettyPrintDeclarations)
import Css.Util exposing (toCssIdentifier, classToString)


{-|-}
prettyPrint : Style class id -> Result String String
prettyPrint style =
    case style of
        Style name declarations ->
            Ok (prettyPrintDeclarations declarations)

        InvalidStyle message ->
            Err message


{- Tags -}

{-| Selector for a html element. -}
html : Tag
html = Tag "html"

{-| Selector for a body element. -}
body : Tag
body = Tag "body"

{-| Selector for a header element. -}
header : Tag
header = Tag "header"

{-| Selector for a nav element. -}
nav : Tag
nav = Tag "nav"

{-| Selector for a div element. -}
div : Tag
div = Tag "div"

{-| Selector for a span element. -}
span : Tag
span = Tag "span"

{-| Selector for a img element. -}
img : Tag
img = Tag "img"

{-| Selector for a nowrap element. -}
nowrap : Tag
nowrap = Tag "nowrap"

{-| Selector for a button element. -}
button : Tag
button = Tag "button"

{-| Selector for an h1 element. -}
h1 : Tag
h1 = Tag "h1"

{-| Selector for an h2 element. -}
h2 : Tag
h2 = Tag "h2"

{-| Selector for an h3 element. -}
h3 : Tag
h3 = Tag "h3"

{-| Selector for an h4 element. -}
h4 : Tag
h4 = Tag "h4"

{-| Selector for a p element. -}
p : Tag
p = Tag "p"

{-| Selector for an ol element. -}
ol : Tag
ol = Tag "ol"

{-| Selector for an input element. -}
input : Tag
input = Tag "input"


tagToString (Tag str) = str


-- TODO these are just for @media - maybe improve type guarantees?
screen = "screen"
print = "print"

-- TODO this is just for ::selection - maybe improve type guarantees?
selection = "selection"


{- Units -}

inheritToString : (a -> String) -> InheritOr a -> String
inheritToString translate value =
    case value of
        Inherit ->
            "inherit"

        NotInherit notInherit ->
            translate notInherit


autoToString : (a -> String) -> AutoOr a -> String
autoToString translate value =
    case value of
        Auto ->
            "auto"

        NotAuto notAuto ->
            translate notAuto

noneToString : (a -> String) -> NoneOr a -> String
noneToString translate value =
    case value of
        None ->
            "none"

        NotNone notNone ->
            translate notNone


unitsToString : Units -> String
unitsToString =
    (\(ExplicitUnits str) -> str)
        |> inheritToString


boxSizingToString : BoxSizing -> String
boxSizingToString =
    (\(ExplicitBoxSizing str) -> str)
        |> inheritToString


overflowToString : Overflow -> String
overflowToString =
    (\(ExplicitOverflow str) -> str)
        |> autoToString
        |> inheritToString


displayToString : Display -> String
displayToString =
    (\(ExplicitDisplay str) -> str)
        |> noneToString
        |> inheritToString


verticalAlignToString : VerticalAlign -> String
verticalAlignToString =
    (\(ExplicitVerticalAlign str) -> str)
        |> inheritToString


whiteSpaceToString : WhiteSpace -> String
whiteSpaceToString =
    (\(ExplicitWhiteSpace str) -> str)
        |> autoToString
        |> inheritToString

colorToString : Color -> String
colorToString =
    (\(ExplicitColor str) -> str)
        |> autoToString
        |> inheritToString


numberToString : number -> String
numberToString num =
    toString (num + 0)


textShadowToString : TextShadow -> String
textShadowToString =
    explicitTextShadowToString
        |> noneToString
        |> inheritToString


explicitTextShadowToString : ExplicitTextShadow -> String
explicitTextShadowToString value =
    case value of
        NoTextShadow ->
            "TODO"

outlineStyleToString : OutlineStyle -> String
outlineStyleToString (OutlineStyle str) = str


opacityStyleToString : OpacityStyle -> String
opacityStyleToString (OpacityStyle str) = str


type Tag
    = Tag String

type InheritOr a
    = Inherit
    | NotInherit a

type AutoOr a
    = Auto
    | NotAuto a

type NoneOr a
    = None
    | NotNone a

type alias BoxSizing = InheritOr ExplicitBoxSizing
type alias Overflow = InheritOr (AutoOr ExplicitOverflow)
type alias Display = InheritOr (NoneOr ExplicitDisplay)
type alias WhiteSpace = InheritOr (AutoOr ExplicitWhiteSpace)
type alias Color = InheritOr (AutoOr ExplicitColor)
type alias TextShadow = InheritOr (NoneOr ExplicitTextShadow)
type alias Outline = InheritOr ExplicitOutline
type alias Units = InheritOr ExplicitUnits
type alias VerticalAlign = InheritOr ExplicitVerticalAlign

type ExplicitUnits = ExplicitUnits String
type ExplicitBoxSizing = ExplicitBoxSizing String
type ExplicitOverflow = ExplicitOverflow String
type ExplicitDisplay = ExplicitDisplay String
type ExplicitWhiteSpace = ExplicitWhiteSpace String
type ExplicitColor = ExplicitColor String
type ExplicitVerticalAlign = ExplicitVerticalAlign String

type ExplicitOutline
    = ExplicitOutline Float ExplicitUnits OutlineStyle OpacityStyle

type OutlineStyle
    = OutlineStyle String

type OpacityStyle
    = OpacityStyle String

type ExplicitTextShadow
    = NoTextShadow

-- Properties --

{-|-}
solid : OutlineStyle
solid = OutlineStyle "solid"

{-|-}
transparent : OpacityStyle
transparent = OpacityStyle "transparent"

{-|-}
rgb : number -> number -> number -> Color
rgb r g b =
    ExplicitColor ("rgb(" ++ (numberToString r) ++ ", " ++ (numberToString g) ++ ", " ++ (numberToString b) ++ ")")
        |> NotAuto |> NotInherit


{-|-}
rgba : number -> number -> number -> number -> Color
rgba r g b a =
    ExplicitColor ("rgba(" ++ (numberToString r) ++ ", " ++ (numberToString g) ++ ", " ++ (numberToString b) ++ ", " ++ (numberToString a) ++ ")")
        |> NotAuto |> NotInherit


{-|-}
hex : String -> Color
hex str =
    ExplicitColor ("#" ++ str)
        |> NotAuto |> NotInherit

{-|-}
pct : Units
pct = "%" |> ExplicitUnits |> NotInherit

{-|-}
em : Units
em = "em" |> ExplicitUnits |> NotInherit

{-|-}
px : Units
px = "px" |> ExplicitUnits |> NotInherit

{-|-}
borderBox : BoxSizing
borderBox = "border-box" |> ExplicitBoxSizing |> NotInherit

{-|-}
visible : Display
visible = "visible" |> ExplicitDisplay |> NotNone |> NotInherit

{-|-}
block : Display
block = "block" |> ExplicitDisplay |> NotNone |> NotInherit

{-|-}
inlineBlock : Display
inlineBlock = "inline-block" |> ExplicitDisplay |> NotNone |> NotInherit

{-|-}
inline : Display
inline = "inline" |> ExplicitDisplay |> NotNone |> NotInherit

{-|-}
none : InheritOr (NoneOr a)
none = None |> NotInherit

{-|-}
auto : InheritOr (AutoOr a)
auto = Auto |> NotInherit

{-|-}
inherit : InheritOr a
inherit = Inherit

{-|-}
noWrap : WhiteSpace
noWrap = "no-wrap" |> ExplicitWhiteSpace |> NotAuto |> NotInherit

{-|-}
top : VerticalAlign
top = "top" |> ExplicitVerticalAlign |> NotInherit

{-|-}
middle : VerticalAlign
middle = "middle" |> ExplicitVerticalAlign |> NotInherit

{-|-}
bottom : VerticalAlign
bottom = "bottom" |> ExplicitVerticalAlign |> NotInherit


{- Properties -}

prop1 key translate value =
    ( key, (translate value) )


prop2 key translateA translateB valueA valueB =
    ( key, (translateA valueA) ++ (translateB valueB) )


prop3 key translateA translateB translateC valueA valueB valueC =
    ( key, (translateA valueA) ++ (translateB valueB) ++ (translateC valueC) )


prop4 key translateA translateB translateC translateD valueA valueB valueC valueD =
    ( key, (translateA valueA) ++ (translateB valueB) ++ (translateC valueC) ++ (translateD valueD) )

prop5 key translateA translateB translateC translateD translateE valueA valueB valueC valueD valueE =
    ( key, (translateA valueA) ++ (translateB valueB) ++ (translateC valueC) ++ (translateD valueD) ++ (translateE valueE) )


{-|-}
verticalAlign : VerticalAlign -> (String, String)
verticalAlign =
    prop1 "vertical-align" verticalAlignToString


{-|-}
display : Display -> (String, String)
display =
    prop1 "display" displayToString


{-|-}
opacity : OpacityStyle -> (String, String)
opacity =
    prop1 "opacity" toString


{-|-}
width : number -> Units -> (String, String)
width =
    prop2 "width" numberToString unitsToString


{-|-}
minWidth : number -> Units -> (String, String)
minWidth =
    prop2 "min-width" numberToString unitsToString


{-|-}
height : number -> Units -> (String, String)
height =
    prop2 "height" numberToString unitsToString


{-|-}
minHeight : number -> Units -> (String, String)
minHeight =
    prop2 "min-height" numberToString unitsToString


{-|-}
padding : number -> Units -> (String, String)
padding =
    prop2 "padding" numberToString unitsToString

{-|-}
paddingTop : number -> Units -> (String, String)
paddingTop =
    prop2 "padding-top" numberToString unitsToString

{-|-}
paddingBottom : number -> Units -> (String, String)
paddingBottom =
    prop2 "padding-bottom" numberToString unitsToString

{-|-}
paddingRight : number -> Units -> (String, String)
paddingRight =
    prop2 "padding-right" numberToString unitsToString

{-|-}
paddingLeft : number -> Units -> (String, String)
paddingLeft =
    prop2 "padding-left" numberToString unitsToString

{-|-}
margin : number -> Units -> (String, String)
margin =
    prop2 "margin" numberToString unitsToString

{-|-}
marginTop : number -> Units -> (String, String)
marginTop =
    prop2 "margin-top" numberToString unitsToString

{-|-}
marginBottom : number -> Units -> (String, String)
marginBottom =
    prop2 "margin-bottom" numberToString unitsToString

{-|-}
marginRight : number -> Units -> (String, String)
marginRight =
    prop2 "margin-right" numberToString unitsToString

{-|-}
marginLeft : number -> Units -> (String, String)
marginLeft =
    prop2 "margin-left" numberToString unitsToString

{-|-}
boxSizing : BoxSizing -> (String, String)
boxSizing =
    prop1 "box-sizing" boxSizingToString


{-|-}
overflowX : Overflow -> (String, String)
overflowX =
    prop1 "overflow-x" overflowToString


{-|-}
overflowY : Overflow -> (String, String)
overflowY =
    prop1 "overflow-y" overflowToString


{-|-}
whiteSpace : WhiteSpace -> (String, String)
whiteSpace =
    prop1 "white-space" whiteSpaceToString


{-|-}
backgroundColor : Color -> (String, String)
backgroundColor =
    prop1 "background-color" colorToString


{-|-}
color : Color -> (String, String)
color =
    prop1 "color" colorToString

{-|-}
media : a -> String
media value =
    "media " ++ (toString value)
    -- TODO

{-|-}
textShadow : TextShadow -> (String, String)
textShadow =
    prop1 "text-shadow" textShadowToString


{-|-}
outline : Float -> Units -> OutlineStyle -> OpacityStyle -> (String, String)
outline =
    prop4
        "outline"
            toString unitsToString
            (\str -> " " ++ outlineStyleToString str ++ " ")
            opacityStyleToString


{- Types -}

{-| A namespaced stylesheet. -}
type Style class id
    = Style String (List Declaration)
    | InvalidStyle String


{-| An empty namespaced stylesheet. Use this as the foundation on which to build
your stylesheet.

    stylesheet "homepage"
        $ body
            ~ width 960 px
            ~ color (rgb 7 7 7)
-}
stylesheet : String -> Style class id
stylesheet name =
    Style name []


{-| A [type selector](https://developer.mozilla.org/en-US/docs/Web/CSS/Type_selectors).

    stylesheet "homepage"
        $ body
            ~ width 960 px
            ~ color (rgb 7 7 7)
-}
($) : Style class id -> Tag -> Style class id
($) style tag =
    case style of
        Style name declarations ->
            let
                declaration =
                    selectorToBlock (TypeSelector (tagToString tag))
            in
                declarations ++ [ declaration ]
                    |> Style name

        InvalidStyle _ ->
            style


{-| An [id selector](https://developer.mozilla.org/en-US/docs/Web/CSS/ID_selectors).

    stylesheet "homepage"
        # NavBar
            ~ width 960 px
            ~ backgroundColor (rgb 123 42 208)
-}
(#) : Style class id -> id -> Style class id
(#) style id =
    case style of
        Style name declarations ->
            let
                declaration =
                    selectorToBlock (IdSelector (toCssIdentifier id))
            in
                declarations ++ [ declaration ]
                    |> Style name

        InvalidStyle _ ->
            style


{-| A [class selector](https://developer.mozilla.org/en-US/docs/Web/CSS/Class_selectors).

    stylesheet "homepage"
        . Hero
            ~ fontWeight bold
            ~ color (rgb 7 7 7)
-}
(.) : Style class id -> class -> Style class id
(.) style class =
    case style of
        Style name declarations ->
            let
                declaration =
                    selectorToBlock (ClassSelector (classToString name class))
            in
                declarations ++ [ declaration ]
                    |> Style name

        InvalidStyle _ ->
            style


{-| An [at-rule](https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule).

    stylesheet "homepage"
        @ charset "utf-8"

        html
            width 100 pct

        @ media screen
            $ body
                width 960 px

        @ media print
            $ body
                width 100 pct
-}
(@) : Style class id -> String -> Style class id
(@) style rule =
    case style of
        Style name declarations ->
            let
                declaration =
                    ConditionalGroupRule rule []
            in
                declarations ++ [ declaration ]
                    |> Style name

        InvalidStyle _ ->
            style

{-| A custom selector. Use this for things like
[attribute selectors](https://developer.mozilla.org/en-US/docs/Web/CSS/Attribute_selectors)
and [universal selectors](https://developer.mozilla.org/en-US/docs/Web/CSS/Universal_selectors).

    stylesheet "homepage"
        $= "* [lang^=en]"
            ~ textDecoration underline
            ~ color (rgb 7 7 7)
-}
($=) : Style class id -> class -> Style class id
($=) style class =
    case style of
        Style name declarations ->
            let
                declaration =
                    selectorToBlock (ClassSelector (classToString name class))
            in
                declarations ++ [ declaration ]
                    |> Style name

        InvalidStyle _ ->
            style


{-| A [property](https://developer.mozilla.org/en-US/docs/Web/CSS/Reference).

    stylesheet "homepage"
        $ body
            ~ width 960 px
            ~ color (rgb 7 7 7)
-}
(~) : Style class id -> ( String, String ) -> Style class id
(~) style ( key, value ) =
    case style of
        Style name declarations ->
            let
                property =
                    { key = key, value = value, important = False }
            in
                case addProperty "~" property declarations of
                    Ok newDeclarations ->
                        Style name newDeclarations

                    Err message ->
                        InvalidStyle message

        InvalidStyle _ ->
            style


{-| An [`!important`](https://developer.mozilla.org/en-US/docs/Web/CSS/Specificity#The_!important_exception)
property.

    stylesheet "homepage"
        $ button
            ~ color (rgb 7 7 7)
            ! fontWeight normal
-}
(!) : Style class id -> ( String, String ) -> Style class id
(!) style ( key, value ) =
    case style of
        Style name declarations ->
            let
                property =
                    { key = key, value = value, important = True }
            in
                case addProperty "!" property declarations of
                    Ok newDeclarations ->
                        Style name newDeclarations

                    Err message ->
                        InvalidStyle message

        InvalidStyle _ ->
            style


{-|
    stylesheet "homepage"
        $ html >$ body
            ~ width 960 px
            ~ color (rgb 7 7 7)
-}
(>$) : Style class id -> Tag -> Style class id
(>$) style tag =
    case style of
        Style name declarations ->
            let
                update selector =
                    tag
                        |> tagToString
                        |> TypeSelector
                        |> SingleSelector
                        |> Child selector
            in
                case extendLastSelector ">$" update declarations of
                    Ok newDeclarations ->
                        Style name newDeclarations

                    Err message ->
                        InvalidStyle message

        InvalidStyle _ ->
            style

{-|
    stylesheet "homepage"
        $ html |$ body
            ~ width 960 px
            ~ color (rgb 7 7 7)
-}
(|$) : Style class id -> Tag -> Style class id
(|$) style tag =
    case style of
        Style name declarations ->
            let
                newSelector =
                    tag
                        |> tagToString
                        |> TypeSelector
                        |> SingleSelector
            in
                case addSelector "|$" newSelector declarations of
                    Ok newDeclarations ->
                        Style name newDeclarations

                    Err message ->
                        InvalidStyle message

        InvalidStyle _ ->
            style



selectorToBlock : Selector -> Declaration
selectorToBlock selector =
    StyleBlock (SingleSelector selector) [] []


addProperty
    :  String
    -> Property
    -> List Declaration
    -> Result String (List Declaration)
addProperty operatorName property declarations =
    case declarations of
        [] ->
            Err (operatorName ++ " cannot be used as the first declaration.")

        declaration :: [] ->
            case declaration of
                StyleBlock firstSelector extraSelectors properties ->
                    let
                        newDeclaration =
                            StyleBlock
                                firstSelector
                                extraSelectors
                                (properties ++ [ property ])
                    in
                        Ok [ newDeclaration ]

                ConditionalGroupRule _ _ ->
                    Err (operatorName ++ " cannot modify a conditional group rule (such as an at-rule). See https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule#Conditional_Group_Rules for more information on conditional group rules.")

                StandaloneAtRule _ _ ->
                    Err (operatorName ++ " cannot modify an at-rule. See https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule for more information on at-rules.")

        first :: rest ->
            case addProperty operatorName property rest of
                Ok result ->
                    Ok (first :: result)

                Err _ as err ->
                    err

extendLastSelector
    :  String
    -> (CompoundSelector -> CompoundSelector)
    -> List Declaration
    -> Result String (List Declaration)
extendLastSelector operatorName update declarations =
    case declarations of
        [] ->
            Err (operatorName ++ " cannot be used as the first declaration.")

        declaration :: [] ->
            case declaration of
                StyleBlock firstSelector extraSelectors properties ->
                    let
                        newDeclaration =
                            StyleBlock
                                (update firstSelector)
                                (List.map update extraSelectors)
                                []

                        newDeclarations =
                            if List.isEmpty properties then
                                -- Don't bother keeping empty declarations.
                                [ newDeclaration ]
                            else
                                [ declaration, newDeclaration ]
                    in
                        Ok newDeclarations

                ConditionalGroupRule _ _ ->
                    Err (operatorName ++ " cannot modify a conditional group rule (such as an at-rule). See https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule#Conditional_Group_Rules for more information on conditional group rules.")

                StandaloneAtRule _ _ ->
                    Err (operatorName ++ " cannot modify an at-rule. See https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule for more information on at-rules.")

        first :: rest ->
            case extendLastSelector operatorName update rest of
                Ok result ->
                    Ok (first :: result)

                Err _ as err ->
                    err

addSelector
    :  String
    -> CompoundSelector
    -> List Declaration
    -> Result String (List Declaration)
addSelector operatorName newSelector declarations =
    case declarations of
        [] ->
            Err (operatorName ++ " cannot be used as the first declaration.")

        declaration :: [] ->
            case declaration of
                StyleBlock firstSelector extraSelectors properties ->
                    let
                        newDeclaration =
                            StyleBlock
                                firstSelector
                                (extraSelectors ++ [ newSelector ])
                                properties
                    in
                        Ok [ newDeclaration ]

                ConditionalGroupRule _ _ ->
                    Err (operatorName ++ " cannot modify a conditional group rule (such as an at-rule). See https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule#Conditional_Group_Rules for more information on conditional group rules.")

                StandaloneAtRule _ _ ->
                    Err (operatorName ++ " cannot modify an at-rule. See https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule for more information on at-rules.")

        first :: rest ->
            case addSelector operatorName newSelector rest of
                Ok result ->
                    Ok (first :: result)

                Err _ as err ->
                    err

updateLast : (a -> a) -> List a -> List a
updateLast update list =
    case list of
        [] ->
            list

        singleton :: [] ->
            [ update singleton ]

        first :: rest ->
            first :: updateLast update rest
