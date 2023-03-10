#!/bin/bash
set -euo pipefail

(cd Shakefile && hpack)
(cd avh4-lib && hpack)
(cd elm-format-markdown && hpack)
(cd elm-format-lib && hpack)
(cd elm-format-test-lib && hpack)
hpack
exec cabal run build-elm-format:exe:build-elm-format -- --share "$@"
