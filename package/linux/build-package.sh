#!/bin/bash

set -euxo pipefail

VERSION="$(git describe --abbrev=8)"
PLATFORM="linux-x64"

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
    ELM_FORMAT="$(stack path --local-install-root)/bin/elm-format"
    cp "$ELM_FORMAT" dist/package-scripts/elm-format
    strip dist/package-scripts/elm-format
    tar zcvf "$BUILD".tgz -C dist/package-scripts elm-format
}

build-flavor
