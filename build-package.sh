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

tar zcvf elm-format-"$VERSION"-mac-x64.tgz -C .cabal-sandbox/bin elm-format
keybase sign --detach-sign elm-format-"$VERSION"-mac-x64.tgz


## Build Windows x64 binary

# cd package/win
# see setup.txt
