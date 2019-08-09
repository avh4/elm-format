#!/bin/bash

set -ex

VERSION="$(git describe --abbrev=8)"
PLATFORM="win-i386"
BINEXT=".exe"

"$(stack path --local-bin)/shake" --version || stack install shake

## Run tests

stack runhaskell Shakefile.hs -- clean
stack runhaskell Shakefile.hs -- build
stack runhaskell Shakefile.hs -- stack-test
stack runhaskell Shakefile.hs -- integration-tests


## Build binaries

rm -Rf .stack-work
stack build --ghc-options='-O2'

function build-flavor() {
    BUILD="elm-format-${VERSION}-${PLATFORM}"
    mkdir -p dist/package-scripts
    ELM_FORMAT="$(stack path --local-install-root)/bin/elm-format${BINEXT}"
    cp "$ELM_FORMAT" "dist/package-scripts/elm-format${BINEXT}"
    strip "dist/package-scripts/elm-format${BINEXT}"
    rm -f "$BUILD".zip
    zip -9 -j "$BUILD".zip "dist/package-scripts/elm-format${BINEXT}"
}

build-flavor
