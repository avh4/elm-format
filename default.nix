{ sources ? import ./nix/sources.nix { }, pkgs ? null }:

let
  haskell-nix = if pkgs == null then
    let
      haskellNix = import sources.haskellNix { };
      finalPkgs =
        import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
    in finalPkgs.haskell-nix

  else
    let haskellNix = import sources.haskellNix { inherit pkgs; };
    in haskellNix.pkgs.haskell-nix;

  pkgSet = haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [ ];
    modules = [ ];
  };

in pkgSet.config.hsPkgs
