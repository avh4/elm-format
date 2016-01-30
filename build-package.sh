#!/bin/bash

set -ex

VERSION="$(sed -ne "s/^Version: //p" elm-format.cabal)"

## Run tests

cabal clean
cabal configure --enable-tests
./tests/run-tests.sh


## Build mac-x64 binary

cabal clean
cabal configure
cabal build
cabal install

BUILD="elm-format-${VERSION}-mac-x64"

tar zcvf "$BUILD".tgz -C .cabal-sandbox/bin elm-format


## Build Windows x64 binary

# cd package/win
# see setup.txt
# vagrant up
## open VirtualBox and show the screen
## Start PowerShell
#># cd Desktop
#># git fetch
#># git checkout ${VERSION}
#># cabal update
#># cabal install --only-dependencies
#># cabal clean
#># cabal configure
#># cabal build
#># cabal install
#># cp .\.cabal-sandbox\bin\elm-format.exe C:\vagrant
## back to host computer
# cd ./package/win/
# vagrant halt


## Build Linux x64 binary

# cd package/linux
# vagrant ssh
#># bash /vagrant/build-package.sh
# vagrant halt


## Collect files

# cd package/win
# zip elm-format-${VERSION}-win-x64.zip elm-format.exe
# cd ../..
# mv package/win/elm-format-${VERSION}-win-x64.zip ../../
# mv package/linux/elm-format-${VERSION}-linux-x64.tgz ./

# for i in elm-format-${VERSION}-{mac-x64.tgz,win-x64.zip,linux-x64.tgz} do
#   keybase pgp sign --detached --infile "$i" --outfile "$i".asc
#   github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz
#   github-release upload --user avh4 --repo elm-format --tag "$VERSION" --file "$BUILD".tgz.asc
# end
