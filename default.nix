let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          indents =
            haskellPackagesNew.callPackage ./nix/indents.nix { };

          quickcheck-io =
            haskellPackagesNew.callPackage ./nix/quickcheck-io.nix { };

          optparse-applicative =
            haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { };

          hspec-core =
            pkgs.haskell.lib.doJailbreak haskellPackagesOld.hspec-core;

          elm-format =
            pkgs.haskell.lib.dontHaddock
              (haskellPackagesNew.callPackage ./nix/elm-format.nix {
                git = pkgs.git;
                coreutils = pkgs.coreutils;
              });
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { elm-format = pkgs.haskellPackages.elm-format;
  }
