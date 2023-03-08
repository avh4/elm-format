#!/bin/bash
set -euo pipefail

(cd avh4-lib && hpack)
exec cabal run build-elm-format:exe:build-elm-format -- --share "$@"
