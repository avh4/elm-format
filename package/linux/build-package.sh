#!/bin/bash
set -euxo pipefail


## Run tests
./dev/build.sh -- clean
./dev/build.sh

## Build binaries
./dev/build.sh -- dist
