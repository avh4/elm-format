#!/bin/bash

set -ex

VERSION="$(sed -ne "s/^Version: //p" elm-format.cabal)"

## Run tests

cabal clean
cabal configure --enable-tests
./tests/run-tests.sh


## Build mac-x64 binary

cabal clean
cabal configure
cabal build
cabal install

BUILD="elm-format-${VERSION}-mac-x64"

tar zcvf "$BUILD".tgz -C .cabal-sandbox/bin elm-format
