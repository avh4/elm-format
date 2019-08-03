module DocCommentCodeExample exposing (f)

{-| -}


{-| Compute with f (indented code block)

    myValue =
        f 20

-}
f x =
    x * x


{-| Compute with g (backticks code block)

    myValue =
        g 99

-}
g y =
    y + (4 * y)


{-| Format expressions in code blocks

    h 20 --> 21

    h 21 --> 22

-}
h z =
    z + 1
