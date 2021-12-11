#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git cabal2nix

set -exo pipefail

REV="$(git rev-parse HEAD)"
ELM_FORMAT_VERSION="$(git describe --abbrev=8)"
BRANCH="elm-format-$ELM_FORMAT_VERSION"

if [ ! -d nixpkgs ]; then
  git clone --depth 10 --branch master https://github.com/NixOS/nixpkgs.git nixpkgs
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

cabal2nix --no-haddock https://github.com/avh4/elm-format --revision "$REV" --subpath avh4-lib > nixpkgs/pkgs/development/compilers/elm/packages/avh4-lib.nix
cabal2nix --no-haddock https://github.com/avh4/elm-format --revision "$REV" --subpath elm-format-lib > nixpkgs/pkgs/development/compilers/elm/packages/elm-format-lib.nix
cabal2nix --no-haddock https://github.com/avh4/elm-format --revision "$REV" --subpath elm-format-test-lib > nixpkgs/pkgs/development/compilers/elm/packages/elm-format-test-lib.nix
cabal2nix --no-haddock https://github.com/avh4/elm-format --revision "$REV" --subpath elm-format-markdown > nixpkgs/pkgs/development/compilers/elm/packages/elm-format-markdown.nix
./generate_derivation.sh > nixpkgs/pkgs/development/compilers/elm/packages/elm-format.nix

pushd nixpkgs
rm -f result
nix-build --option sandbox true -A elmPackages.elm-format
git status
result/bin/elm-format | head -n1
popd

set +x

echo
echo "Everything looks good!"
echo "You will need to make a PR from the $BRANCH branch"
