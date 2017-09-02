{ compiler ? "ghc801" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              elm-format = haskellPackagesNew.callPackage ./default.nix {
                git-cli = pkgs.git;
              };

              indents = haskellPackagesNew.callPackage ./nix/indents.nix { };
              optparse-applicative = haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { };
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { elm-format = pkgs.haskell.packages.${compiler}.elm-format;
  }