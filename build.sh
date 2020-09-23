#!/bin/bash
set -euo pipefail

if [ ! -f _build/build ]; then
  mkdir -p _build
  cabal new-install --lib shake
fi
ghc --make Shakefile.hs -package shake -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_build -o _build/build && _build/build "$@"
