{ compiler ? "ghc801" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: rec {
              elm-format = self.callPackage ./default.nix {
                git-cli = pkgs.git;
              };

              indents = self.callPackage ./nix/indents.nix { };
              optparse-applicative = self.callPackage ./nix/optparse-applicative.nix { };
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