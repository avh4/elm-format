#!/bin/bash

set -exo pipefail

VERSION="$(git describe --abbrev=8)"

for i in dist/elm-format-${VERSION}-{mac-x64.tgz,win-i386.zip,linux-x64.tgz}; do
  keybase pgp sign --detached --infile "$i" --outfile "$i".asc
  # github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz
  # github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz.asc
done
