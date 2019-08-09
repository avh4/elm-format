module DocCommentMarkdownSafety exposing (x)

{-| These are examples where the input markdown is unsafe (meaning it
is parsed differently by some popular markdown implementations).

We transform these cases to use markdown syntax that is more consistent
across parsers.
-}


{-| Indented code after an indented list.

  - Item 1
  - Item 2


    myValue = f 20
-}
f x =
    x * x


{-| Test

  - Item 1
  - Item 2


    Code ...
-}
x = ()
