#!/bin/bash

set -ex

cabal update
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
./tests/run-tests.sh
