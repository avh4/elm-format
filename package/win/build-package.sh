#!/bin/bash

set -ex

VERSION="$(git describe --abbrev=8)"
PLATFORM="win-i386"
BINEXT=".exe"

## Run tests

stack runhaskell Shakefile.hs -- clean
stack runhaskell Shakefile.hs -- -j4 --lint


## Build binaries

stack clean
stack build --ghc-options='-O2'

function build-flavor() {
    BUILD="elm-format-${VERSION}-${PLATFORM}"
    mkdir -p dist/package-scripts
    ELM_FORMAT="$(stack path --local-install-root)/bin/elm-format${BINEXT}"
    cp "$ELM_FORMAT" "dist/package-scripts/elm-format${BINEXT}"
    tar zcvf "$BUILD".tgz -C dist/package-scripts "elm-format${BINEXT}"
}

build-flavor
