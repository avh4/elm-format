{ ... }:
let
  sources = import ./nix/sources.nix;

  niv = import sources.niv { };

  nixpkgs = import sources.nixpkgs { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elm-format";
  buildInputs = [ niv.niv stack libiconv ];
}
