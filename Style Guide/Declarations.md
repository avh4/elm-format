# Declarations

<!-- toc -->


## Infix operators

Avoid infix operators when possible and prefer using named functions.

Infix operator declartions should prefer the following ordering:
documentation comment,
infix properties (if not the default),
type annotation,
definition.

```elm
{-| Documentation
-}
infixr 5 ::?
(::?) : Maybe a -> List a -> List a
(::?) elem list =
    list
```
