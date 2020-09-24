#!/bin/bash
set -euxo pipefail

## Run tests
./build.sh -- clean
./build.sh

## Build binaries
./build.sh -- dist
