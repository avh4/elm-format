# Expressions

<!-- toc -->


## Binary operators

Parentheses are not used around function application within binary operator expressions:

```elm
"a" ++ f x y ++ "b"  -- no parens around (f x y)
```

Parentheses are used around `if`, `case`, `let`, and lambda expressions:

```elm
foo bool =
    "A"
        ++ (case bool of
                True ->
                    "true"

                False ->
                    "false"
           )
```

However, you should prefer refactoring your code to using complex expressions
within binary operator expressions:

```elm
foo bool =
    let
        truthString =
            case bool of
                True ->
                    "true"

                False ->
                    "false"
    in
    "A" ++ truthString
```


## Left pipe operator

Unlike other binary operators, the left pipe `<|` operator is
places at the end of the preceding line, and the following lines are indented:

```elm
Ok <|
    if b then
        "YES"
    else
        "NO"
```

Parentheses are not used around the second term unless it is a binary operator expression:

```elm
Ok <| ([1,2,3] |> List.tail)  -- parens are required

Ok <| ("a" ++ "b")  -- parens are used

test "should pass" <|
    \() ->
        Expect.pass  -- parens are not used around the lambda
```



## Parentheses

Parentheses are required in the following places:

  - around `let`, `if`, `case`, and lambda expressions
    within function application and infix operator expressions

Parentheses are allowed (but not required) in the following places:

  - within sequences of infix operator expressions

    ```elm
    (1 + 2) * (3 - (4 - 1))
    ```

Notably, parentheses are not used in the following places:

  - around the predicate of `if` expressions
  - around the subject of `case` expressions
  - around function application within infix operator expressions

    ```elm
    "a" ++ f x y ++ "b" -- no parens around (f x y)
    ```
  - around the field values in record expressions
