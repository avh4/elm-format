{ ... }:
let
  sources = import ./nix/sources.nix;

  niv = import sources.niv { };

  nixpkgs = import sources.nixpkgs { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elm-format";
  buildInputs = [
    # Tools required to build elm-format
    stack git python3

    # Tools for updating nix config
    niv.niv

    # Tools needed by stack
    libiconv gcc perl
  ];
}
