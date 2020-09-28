#!/bin/bash

set -euo pipefail

if [[ $(docker --version) == *" 19.0"[3-9]* ]]; then
    true
else
    echo "ERROR: Docker >= 19.03 is required"
    docker --version
    exit 1
fi

set -x

export DOCKER_BUILDKIT=1

rm -Rf _build/docker/
docker build -t elm-format-dev-linux --target artifact --output type=local,dest=_build/docker/ .
_build/docker/elm-format --help
