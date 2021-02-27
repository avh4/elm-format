module Expressions exposing (caseExpression, charLiteral, emptyList, externalReference, floatLiteral, functionApplication, glShader, intLiteral, lambda, letExpression, list, operator, parensAreAddedWhereNeeded, record, recordAccessFunction, recordUpdate, stringLiteral, tag, tuple, unary, unit, var)


unit =
    ()


intLiteral =
    1


floatLiteral =
    2.0


stringLiteral =
    "string"


charLiteral =
    'c'


var =
    x


operator =
    (+)


tag =
    MyTag


externalReference =
    List.map


functionApplication =
    f (g ())


parensAreAddedWhereNeeded =
    f () ()


unary =
    -x


emptyList =
    []


list =
    [ ()
    , ()
    ]


tuple =
    ( ()
    , ()
    )


record =
    { x = ()
    , y = ()
    }


recordUpdate =
    { r
        | x = ()
        , y = ()
    }


recordAccessFunction =
    .x


lambda =
    \a b -> ()


caseExpression =
    case x of
        () ->
            ()

        _ ->
            ()


letExpression =
    let
        x =
            ()
    in
    ()


glShader =
    [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform   mat4 view;
varying   vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]
