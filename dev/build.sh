#!/bin/bash
set -euo pipefail

(cd Shakefile && hpack)
(cd avh4-lib && hpack)
exec cabal run build-elm-format:exe:build-elm-format -- --share "$@"
