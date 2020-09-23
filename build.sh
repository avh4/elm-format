#!/bin/bash
set -euo pipefail

if [ ! -f _build/build ]; then
  mkdir -p _build
  ghcup set ghc 8.8.4 || ghcup install ghc 8.8.4
  cabal new-install --lib shake
fi
ghc --make Shakefile.hs -package shake -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_build -o _build/build && _build/build "$@"
