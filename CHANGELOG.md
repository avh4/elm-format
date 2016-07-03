## 0.4.0-alpha

Syntax changes:
  - multiline expressions with long infix operators now indent in a more appropriate way
  - The `<|` operator is now handled specially and is placed at the end of the preceding line
  - line breaks are now allowed before the first arguments to functions

Other changes:
 - console output is less verbose
 - files are not touched if they are already formatted
 - elm-format binaries are smaller
 - qualified type constructors in pattern match arguments are now handled correctly w/r to elm-compiler 0.17


## 0.3.1-alpha

 - BUG: Fixed formatting of the following code (in 0.3.0-alpha it would format to invalid syntax) [#179](https://github.com/avh4/elm-format/issues/179)

        import Dict as D
            exposing
                ( empty
                , fromList
                )


## 0.3.0-alpha

See https://github.com/avh4/elm-format/releases/tag/0.3.0-alpha
