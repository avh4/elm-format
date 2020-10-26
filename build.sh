#!/bin/bash
set -euo pipefail

PKG_ENV_FILE=_build/shake-package-env

mkdir -p _build
# install shake if it's not installed
if ! grep -qs '^package-id \(shake\|shk\)-' "$PKG_ENV_FILE"; then
  echo "$0: installing shake"
  cabal v2-install --package-env "$PKG_ENV_FILE" --lib shake
fi

# compile the build script
ghc --make Shakefile.hs -package-env "$PKG_ENV_FILE" -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_build -o _build/build

# run it
_build/build "$@"
