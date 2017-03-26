#!/bin/bash

set -ex

VERSION="$(git describe --abbrev=8)"
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
    ELM_FORMAT="$(stack path --local-install-root)/bin/elm-format-${FLAVOR}"
    cp "$ELM_FORMAT" dist/package-scripts/elm-format
    tar zcvf "$BUILD".tgz -C dist/package-scripts elm-format
}

build-flavor 0.18
build-flavor 0.17
