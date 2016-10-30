#!/bin/bash

set -ex

VERSION="$(sed -ne "s/^Version: //p" elm-format.cabal)"
PLATFORM="mac-x64"

## Run tests

stack clean
./tests/run-tests.sh


## Build binaries

stack build

function build-flavor() {
    FLAVOR="$1"
    BUILD="elm-format-${FLAVOR}-${VERSION}-${PLATFORM}"
    mkdir -p dist/package-scripts
    ELM_FORMAT="`ls ./.stack-work/install/*/lts-7.4/8.0.1/bin/elm-format-${FLAVOR} | head -n1`"
    cp "$ELM_FORMAT" dist/package-scripts/elm-format
    tar zcvf "$BUILD".tgz -C dist/package-scripts elm-format
}

build-flavor 0.18
build-flavor 0.17
build-flavor 0.16
