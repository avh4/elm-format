#!/bin/bash

set -ex

VERSION="$(sed -ne "s/^Version: //p" elm-format.cabal)"


## Collect files

pushd package/win/elm-format
tar zxvf "elm-format-0.16-${VERSION}-win-x64.tgz"
zip "elm-format-0.16-${VERSION}-win-x64.zip" elm-format.exe
tar zxvf "elm-format-0.17-${VERSION}-win-x64.tgz"
zip "elm-format-0.17-${VERSION}-win-x64.zip" elm-format.exe
tar zxvf "elm-format-0.18-${VERSION}-win-x64.tgz"
zip "elm-format-0.18-${VERSION}-win-x64.zip" elm-format.exe
popd

cp -v package/win/elm-format/elm-format-*-${VERSION}-win-x64.zip ./
cp -v package/linux/elm-format/elm-format-*-${VERSION}-linux-x64.tgz ./

for i in elm-format-{0.16,0.17,0.18}-${VERSION}-{mac-x64.tgz,win-x64.zip,linux-x64.tgz}; do
  keybase pgp sign --detached --infile "$i" --outfile "$i".asc
  # github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz
#   github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz.asc
done
