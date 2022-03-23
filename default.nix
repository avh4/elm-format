{ sources ? import ./nix/sources.nix { } }:

let
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;

  pkgSet = pkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [ ];
    modules = [ ];
  };

in pkgSet.config.hsPkgs
