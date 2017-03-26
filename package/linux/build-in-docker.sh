#!/bin/bash

set -ex

docker build -t elm-format-dev-linux .
docker run -v "$(pwd)":/elm-format -w /elm-format elm-format-dev-linux ./package/linux/build-package.sh
