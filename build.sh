#!/bin/bash
set -euo pipefail

# build and run it
exec cabal run build -- "$@"
