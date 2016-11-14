#!/bin/bash

set -ex

VERSION="$(sed -ne "s/^Version: //p" elm-format.cabal)"
PLATFORM="win-x64"
BINEXT=".exe"

# ## Run tests
#
# stack clean
# ./tests/run-tests.sh


## Build binaries

stack build

function build-flavor() {
    FLAVOR="$1"
    BUILD="elm-format-${FLAVOR}-${VERSION}-${PLATFORM}"
    mkdir -p dist/package-scripts
    ELM_FORMAT="`stack path --local-install-root`/bin/elm-format-${FLAVOR}${BINEXT}"
    cp "$ELM_FORMAT" "dist/package-scripts/elm-format{$BINEXT}"
    tar zcvf "$BUILD".tgz -C dist/package-scripts "elm-format${BINEXT}"
}

build-flavor 0.18
build-flavor 0.17
build-flavor 0.16
