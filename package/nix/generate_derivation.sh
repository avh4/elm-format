#! /usr/bin/env nix-shell
#! nix-shell -i bash -p git cabal2nix nix-prefetch-git jq

# This script generates the nix derivation for elm-format intended for nixpkgs:
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/compilers/elm/packages/elm-format.nix

# To use this script, run:
#
# $ ./generate_derivation.sh > elm-format.nix
#
# This might take a bit of time if the dependencies are not already in the nix
# store. If you already have all the dependencies installed, feel free to remove
# them from the shebang to speed up the process.

set -exo pipefail

REV="$(git rev-parse HEAD)"
VERSION="$(git describe --abbrev=8 "$REV")"

PATCH=$(cat <<END
  postPatch = ''
    mkdir -p ./generated
    cat <<EOHS > ./generated/Build_elm_format.hs
    module Build_elm_format where

    gitDescribe :: String
    gitDescribe = "$VERSION"
    EOHS
  '';
END
)

# quoteSubst from https://stackoverflow.com/a/29613573
quoteSubst() {
  IFS= read -d '' -r < <(sed -e ':a' -e '$!{N;ba' -e '}' -e 's/[&/\]/\\&/g; s/\n/\\&/g' <<<"$1")
  printf %s "${REPLY%$'\n'}"
}

cabal2nix --no-haddock https://github.com/avh4/elm-format --revision "$REV" |
  sed "s#isLibrary = true#isLibrary = false#" |
  sed "s#^}\$#$(quoteSubst "$PATCH")\n}#"
