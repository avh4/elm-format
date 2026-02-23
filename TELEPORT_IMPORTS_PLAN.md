# [Issue #512] Teleport Imports Feature Plan

## Goal
Allow `import` statements to appear anywhere in an Elm module file (e.g., after top-level declarations), and have elm-format move ("teleport") them to the standard import block at the top of the module.

## Current Architecture

- **Parser**: `elm-format-lib/src/Parse/Module.hs` — `elmModule` parses module header -> imports -> declarations in strict sequence. The `imports` function uses `many` to collect all consecutive `import` lines, then hands off to `topLevel` for declarations. An `import` appearing after a declaration causes a parse error.

- **AST**: `elm-format-lib/src/AST/Module.hs` — `Module` has separate fields:
  - `imports :: C1 'BeforeTerm (Map ns (C1 'BeforeTerm ImportMethod))` — the import block
  - `body :: body` — the declarations (a `TopLevel [TopLevelStructure ...]`)

- **Render**: `elm-format-lib/src/ElmFormat/Render/Box.hs` — `formatModule` renders initial comments, module header (including imports), then the body. Imports are rendered from the `imports` field of `Module`.

- **Pipeline**: `src/ElmFormat/Cli.hs` — `format` calls `Parse.parse` -> `Render.render`. No intermediate transform step exists (transforms are baked into the renderer).

- **Tests**: Transform tests live in `tests/test-files/transform/Elm-0.19/`. Each test is a pair: `Foo.elm` (input) + `Foo.formatted.elm` (expected output). The Shake build system runs elm-format on the input and diffs against the expected output.

## Implementation Steps

- [x] **1. Modify the parser to accept imports within the declaration body**
  - Added `topLevelWithImports` and `freshDefOrImport` functions in `Parse/Module.hs` that recognize `import` statements as valid entries within the declaration sequence.
  - Displaced imports are parsed using the existing `import'` parser and collected separately from declarations.

- [x] **2. Handle displaced imports at parse time (Option B)**
  - Displaced imports are collected during the `topLevelWithImports` phase and merged into the main `imports` map before constructing the `Module`.
  - No AST changes needed — the rest of the pipeline (renderer) works unchanged.

- [x] **3. Implement the parser changes**
  - Modified `elmModule` in `Parse/Module.hs` to use `topLevelWithImports` instead of `topLevel`.
  - Used `located` to track source positions for the body.
  - Added `mergeDisplacedImports` to merge extra imports into the existing import map.
  - Extracted `mergeImport` as a shared helper used by both `imports` and `mergeDisplacedImports`.
  - The original `topLevel` function is preserved unchanged (it's exported and used by `parseDeclarations` and `parseExpressions`).

- [x] **4. Create test input files**
  - Created `tests/test-files/transform/Elm-0.19/TeleportImports.elm` — multiple displaced imports with `exposing` and `as` clauses.
  - Created `tests/test-files/transform/Elm-0.19/TeleportImportsMerge.elm` — displaced import that merges with an existing import.

- [x] **5. Create expected output files**
  - Created `tests/test-files/transform/Elm-0.19/TeleportImports.formatted.elm`
  - Created `tests/test-files/transform/Elm-0.19/TeleportImportsMerge.formatted.elm`

- [x] **6. Build and run the transform tests**
  - `cabal build elm-format` succeeds.
  - All 13 Elm-0.19 transform tests pass (11 existing + 2 new).
  - All 15 Elm-0.19 "good" (idempotency) tests pass.
  - `cabal test elm-format-tests` passes (unit tests including property tests).

- [x] **7. Verify the output compiles with the Elm compiler**
  - Formatted output of a file with displaced imports compiles successfully with `elm make` (Elm 0.19.1).
  - The unformatted file (with displaced imports) does NOT compile with Elm (expected — Elm rejects `import` in the body).

- [x] **8. Handle edge cases**
  - Multiple displaced imports: PASS
  - Displaced imports with `as` aliases and `exposing` clauses: PASS
  - Displaced imports that duplicate existing imports (merges correctly): PASS
  - Comments before displaced imports (comments stay in body, import teleports): PASS
  - No initial imports, only displaced imports: PASS
  - Normal files without displaced imports (regression): PASS

## Files Changed

- `elm-format-lib/src/Parse/Module.hs` — Parser changes to accept and teleport displaced imports
- `tests/test-files/transform/Elm-0.19/TeleportImports.elm` — Test input
- `tests/test-files/transform/Elm-0.19/TeleportImports.formatted.elm` — Test expected output
- `tests/test-files/transform/Elm-0.19/TeleportImportsMerge.elm` — Test input (merge case)
- `tests/test-files/transform/Elm-0.19/TeleportImportsMerge.formatted.elm` — Test expected output (merge case)

[Issue #512]: https://github.com/avh4/elm-format/issues/512
