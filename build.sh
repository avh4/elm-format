#!/bin/bash
set -euo pipefail

# install shake if it's not installed
if [ ! -e "$(stack path --local-bin)/shake" ]; then
    stack install shake
fi

# compile the build script
mkdir -p _build
stack ghc -- --make Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_build -o _build/build

# run it
_build/build "$@"
