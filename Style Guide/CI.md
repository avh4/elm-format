# Continous Integration

You can ensure that changes to your project are properly formatted
by using `elm-format` in your contiunous integration builds.

```sh
elm-format --validate .
```

The `--validate` option causes `elm-format` to do the following:

  - No files will be changed on disk
  - There will be no interactive prompts
  - If all the specified files are properly formatted, it will exit successfully (exit code 0).
    If one or more specified files are not properly formatted, it will exit with exit code 1.
  - The output will be a JSON list of errors (or an empty list if all Elm files are properly formatted)

The `.` argument tells `elm-format` to search the current directory recursively for Elm files.
This will find all `*.elm` files, ignoring any directories named `elm-stuff` or `node_modules`.

## JSON format


### `elm-format --validate` output

The output will be a `List` of [FormattingErrors](#FormattingError).


### FormattingError

A FormattingError will be an object with the following fields:

  - **path**: `String`.  The path of the file that is not properly formatted.
    This will be a relative path if the path(s) passed to `elm-format` were relative paths.
  - **message**: `String`.  A message describing the error.


### Example JSON

```json
[{"path":"./src/Fifo.elm","message":"File is not formatted with elm-format-0.8.2 --elm-version=0.19"}
,{"path":"./tests/Tests.elm","message":"File is not formatted with elm-format-0.8.2 --elm-version=0.19"}
]
```
