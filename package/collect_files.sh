#!/bin/bash

set -ex

VERSION="$(git describe --abbrev=8)"


## Collect files

pushd package/win/elm-format
tar zxvf "elm-format-0.18-${VERSION}-win-i386.tgz"
zip "elm-format-0.18-${VERSION}-win-i386.zip" elm-format.exe
tar zxvf "elm-format-0.19-${VERSION}-win-i386.tgz"
zip "elm-format-0.19-${VERSION}-win-i386.zip" elm-format.exe
popd

cp -v package/win/elm-format/elm-format-*-"${VERSION}"-win-i386.zip ./

for i in elm-format-{0.18,0.19}-${VERSION}-{mac-x64.tgz,win-i386.zip,linux-x64.tgz}; do
  keybase pgp sign --detached --infile "$i" --outfile "$i".asc
  # github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz
#   github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz.asc
done
