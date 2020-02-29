#!/bin/bash

set -ex

VERSION="$(git describe --abbrev=8)"

pushd package/win/elm-format
tar zxvf "elm-format-${VERSION}-win-i386.tgz"
zip "elm-format-${VERSION}-win-i386.zip" elm-format.exe
popd

cp -v package/win/elm-format/elm-format-"${VERSION}"-win-i386.zip ./

