## 0.8.2

New features:
  - Invalid `(..)` in `@docs` lines are automatically removed.
  - When not specifying `--elm-version`, if auto-detection fails, Elm 0.19 will be assumed instead of failing with an error.
  - `module` lines with no `exposing` clause will have the clause automatically generated.
  
Feature changes:
  - `exposing (..)` in `module` lines is no longer expanded.

Bug fixes:
  - Error messages are now correctly written to stderr.
  - Literal tabs are now allowed in String and Char literals, as is allowed by Elm 0.19.
  - Char literals immediately following variable names are now correctly handled for Elm 0.19.
  - Empty record patterns (allowed by Elm 0.19) are now correctly handled.
  - Doc comment code blocks containing only a valid Elm `module` line no longer cause a crash.
  - `module` lines in doc comment code blocks are no longer removed.


## 0.8.1

New features:
  - `elm-format` now automatically detects your Elm version based on your `elm.json` or `elm-package.json`.
    You can use the `--elm-version` option if the automatic detection fails.
  - Missing parens are automatically added for import lines

Bug fixes:
  - URLs in doc comments containing special markdown characters are handled more correctly
  - For Elm 0.18 and earlier, tag listings of documented custom types are no longer converted to `(..)`
  - Listing a value more than once in module documentation no longer results in an invalid module line
  - Code blocks in doc comments containing commented Elm expressions are now correctly separated by a single blank line
  - When converting `exposing (..)` to an explicit listing, undocumented values are no longer hidden


## 0.8.0

Features promoted from 0.7.0-exp:
  - Imports are now sorted and duplicate imports are merged
  - `exposing` clauses in module headers are now sorted
  - The body of `let` expressions are no longer indented
  - Unnecessary parentheses are now removed (except in binary operator expressions)

Other new features:
  - Added support for Elm 0.19
  - Added Elm 0.19 support for `--upgrade`
  - Expressions in doc comment code blocks are now formatted

Bug fixes:
  - Parentheses are no longer removed if doing so would cause a comment to move to a different AST node
  - Multiline `@docs` lines in markdown are now handled correctly
  - Trailing whitespace in multiline strings is no longer removed
  - Special characters in doc comments are now escaped more correctly
  - On Windows, special folders are now correctly skipped when traversing the filesystem
  - Initial comments in doc comment code blocks are no longer removed

Syntax changes:
  - Infix operator precedence and associativity declarations are now grouped together
  - `if` expressions now have blank lines between clauses
  - Comments immediately following `import`s now have spacing consistent with other top-level comments
  - `module` lines now have `exposing` at the end of the line, and exposed listings are indented less when multiline
  - `module` lines now sort and group the `exposing` clause based on `@docs` lines in the module documentation
  - `module exposing (..)` is replaced with an explicit list based on the documentation of the module

Other changes:
  - Removed support for Elm 0.16
  - Installing with `npm install --ignore-scripts` is now supported


## 0.7.0-exp

Experimental changes:
  - Imports are now sorted and duplicate imports are merged
  - `exposing` clauses in module headers are now sorted
  - The body of `let` expressions are no longer indented
  - Unnecessary parentheses are now removed

You can give feedback about experimental features at <https://goo.gl/forms/kLdTN1yikfOI8ZuA3>.

Bug fixes:
  - Initial `@docs` lines in module documentation are now handled correctly
  - Formatting for lambdas with multiline patterns is now implemented
  - Code blocks in doc comments that immediately follow lists are now handled correctly

Other changes:
  - elm-format now has a contributor code of conduct


## 0.6.1-alpha

  - elm-format now formats your documentation comments:
      - code snippets in your documentation will be elm-formatted
      - markdown in your documentation will be normalized


## 0.6.0-alpha

Notes:
  - Support for Elm 0.16 is deprecated (but is still available with the `--elm-version=0.16` option).

Syntax changes:
  - Removed a workaround for an Elm 0.16 compiler bug that added extra parenthesis to qualified type tags in pattern matches
  - End-of-line `--` comments are now kept on their original line when appropriate
  - `--` comments in the middle of binary operator sequences no longer push the following expression to the next line
  - `--` comments can be use to create sections in records and lists
  - For Windows, CRLF newlines no longer corrupt literal strings

Bug fixes:
  - Empty records containing multiline comments are now handled correctly
  - Double quotes at the end of multiline strings are now handled correctly
  - The `where` clause in `effect module`s are now required to have at least one field (and comments are now handled correctly)
  - Record expressions with a trailing comma are no longer allowed (and comments are now handled correctly)
  - Block comments containing only multiple lines of whitespace no longer crash elm-format

Other changes:
  - `elm-format --validate` (meant for use in CI scripts) now reports errors as JSON
  - When recursively searching a directory, `node_modules` folders are ignored


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
