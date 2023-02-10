{ sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs { } }:
let haskellPackages = pkgs.haskell.packages.ghc925;
in haskellPackages.shellFor {
  packages = p: [ ];
  buildInputs = with pkgs; [
    # Tools required to build elm-format
    cabal-install
    git
    python3
    jq

    # Dev tools
    ghcid
    cabal2nix
    niv
    nixfmt
    haskellPackages.haskell-language-server
  ];
}
