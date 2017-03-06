## master (next release, probably 0.6.0-alpha)

Bug fixes:
  - Empty records containing multiline comments are now handled correctly
  - Double quotes at the end of multiline strings are now handled correctly
  - The `where` clause in `effect module`s are now required to have at least one field (and comments are now handled correctly)
  - Record expressions with a trailing comma are no longer allowed (and comments are now handled correctly)


## 0.5.2-alpha

Bug fixes:
  - When upgrading backticks, parentheses are correctly added if the second argument is a function call
  - Added a workaround for an elm-compiler bug where patterns with literal negative numbers cannot be used without parentheses in case expressions


## 0.5.1-alpha

Bug fixes:
  - When upgrading ranges to `List.range`, parentheses are correctly added if the range is used as a function call argument
  - Correctly space top-level declarations that use pattern destructuring


## 0.5.0-alpha

Support for Elm 0.18:
  - Added the `--upgrade` option to help migration code from Elm 0.17 to Elm 0.18
    - Infix function calls using backticks become normal functions calls
    - Infix function calls using backticks with `andThen` and `onError` become pipelines
    - Ranges become calls to `List.range`
    - Primes in variable names become underscores
    - References to `fst` and `snd` become `Tuple.first` and `Tuple.second`

Bug fixes:
  - Type tags starting with `True` and `False` are now handled correctly


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
