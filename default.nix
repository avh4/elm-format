{ sources ? import ./nix/sources.nix { } }:
let
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  src = ./.;
  compiler-nix-name = "ghc901";
}
