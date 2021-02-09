#!/bin/bash

set -euo pipefail

if [[ $(docker --version) == *" 19.0"[0-2]* ]]; then
    echo "ERROR: Docker >= 19.03 is required"
    docker --version
    exit 1
fi

set -x

export DOCKER_BUILDKIT=1

SHA="${1-}"
if [ -z "$SHA" ]; then
    CONTEXT="."
    VERSION="$(git describe --abbrev=8)"
    DEST="_build/docker/local/linux-x64"
else
    CONTEXT="-"
    VERSION="$(git describe --abbrev=8 "$SHA")"
    DEST="_build/docker/$SHA/linux-x64"
fi

rm -Rf "$DEST"
git archive --format=tar.gz "$SHA" | \
    docker build -t elm-format-dev-linux --build-arg "ELM_FORMAT_VERSION=$VERSION" --target artifact --output type=local,dest="$DEST/" "$CONTEXT"
"$DEST/elm-format" --help
