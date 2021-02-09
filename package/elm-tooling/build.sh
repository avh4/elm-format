#!/bin/bash
set -euxo pipefail

ELM_FORMAT_VERSION="$(git describe --abbrev=8)"
BRANCH="elm-format-$ELM_FORMAT_VERSION"

if [ ! -d elm-tooling ]; then
  git clone https://github.com/elm-tooling/elm-tooling-cli.git elm-tooling
fi

pushd elm-tooling
git fetch origin main
git reset --hard
if git branch | grep " ${BRANCH}$"; then
  git checkout "$BRANCH"
  git reset --hard origin/main
else
  git checkout -b "$BRANCH" origin/main
  git reset --hard
fi
popd


(
  sed -e '/^  "elm-format":/,$d' elm-tooling/helpers/known-tools.ts
  sed -e '/^  "elm-format":/,/^  }/p;d' elm-tooling/helpers/known-tools.ts | head -n-1
  ./generate.sh "$ELM_FORMAT_VERSION" | sed -e 's/^/    /'
  echo "  },"
  sed -e '1,/^  "elm-format":/d;1,/^  }/d' elm-tooling/helpers/known-tools.ts
) > tmp
mv tmp elm-tooling/helpers/known-tools.ts

pushd elm-tooling
./node_modules/.bin/jest --updateSnapshot
popd