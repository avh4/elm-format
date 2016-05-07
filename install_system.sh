#!/bin/bash

set -ex

./tests/run-tests.sh
cabal install

mv -i .cabal-sandbox/bin/elm-format /usr/local/bin/elm-format

/usr/local/bin/elm-format --version 2>&1 | head -n1
which elm-format
