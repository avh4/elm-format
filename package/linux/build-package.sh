#!/bin/bash

set -ex

VERSION="$(sed -ne "s/^Version: //p" elm-format.cabal)"

cabal update
cabal sandbox init
cabal install --only-dependencies --enable-tests

## Run tests

cabal clean
cabal configure --enable-tests
./tests/run-tests.sh


## Build linux-x64 binary

cabal clean
cabal configure
cabal build
# cabal install

BUILD="elm-format-${VERSION}-linux-x64"

tar zcvf "$BUILD".tgz -C dist/build/elm-format elm-format
mv "$BUILD".tgz /vagrant/
