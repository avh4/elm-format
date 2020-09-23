#!/bin/bash
set -euxo pipefail

## Run tests
./build.sh -- clean
./build.sh -- build

## Build binaries
./build.sh -- dist
