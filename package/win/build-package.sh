#!/bin/bash

set -ex

VERSION="$(sed -ne "s/^Version: //p" elm-format.cabal)"
PLATFORM="win-x64"
BINEXT=".exe"

# ## Run tests
#
# cabal clean
# cabal configure --enable-tests
# ./tests/run-tests.sh


## Build binaries

# cabal clean
# cabal configure
# cabal build
# cabal install

function build-flavor() {
    FLAVOR="$1"
    BUILD="elm-format-${FLAVOR}-${VERSION}-${PLATFORM}"
    mkdir -p dist/package-scripts
    cp ".cabal-sandbox/bin/elm-format-${FLAVOR}${BINEXT}" "dist/package-scripts/elm-format${BINEXT}"
    tar zcvf "$BUILD".tgz -C dist/package-scripts "elm-format${BINEXT}"
}

build-flavor 0.17
build-flavor 0.16
