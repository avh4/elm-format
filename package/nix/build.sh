#!/bin/bash

set -exo pipefail

ELM_FORMAT_VERSION="$(git describe --abbrev=8)"
BRANCH="elm-format-$ELM_FORMAT_VERSION"

if [ ! -d nixpkgs ]; then
  git clone https://github.com/NixOS/nixpkgs.git nixpkgs
fi

pushd nixpkgs
git fetch origin master
git reset --hard
if git branch | grep " ${BRANCH}$"; then
  git checkout "$BRANCH"
  git reset --hard origin/master
else
  git checkout -b "$BRANCH" origin/master
  git reset --hard
fi
popd

cabal2nix --no-check cabal://indents-0.3.3 > nixpkgs/pkgs/development/compilers/elm/packages/indents.nix
./generate_derivation.sh > nixpkgs/pkgs/development/compilers/elm/packages/elm-format.nix

pushd nixpkgs
rm -f result
nix-build -A elmPackages.elm-format
git status
result/bin/elm-format | head -n1
popd

set +x

echo
echo "Everything looks good!"
echo "You will need to make a PR from the $BRANCH branch"
