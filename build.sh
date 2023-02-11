#!/bin/bash
set -euo pipefail

exec cabal run build-elm-format:exe:build-elm-format -- "$@"
