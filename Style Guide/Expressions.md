# Expressions

<!-- toc -->


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
