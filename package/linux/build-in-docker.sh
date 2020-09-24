#!/bin/bash

set -ex

./build.sh -- clean
docker build -t elm-format-dev-linux .
docker run -v "$(pwd)":/elm-format -w /elm-format elm-format-dev-linux ./package/linux/build-package.sh
docker run -v "$(pwd)":/elm-format -w /elm-format elm-format-dev-linux ./build.sh -- clean
