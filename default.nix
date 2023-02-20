{ nixpkgs ? <nixpkgs>, compiler ? "ghc925" }:

let
  haskellPackageOverrides = pkgs: self: super:
    with pkgs.haskell.lib; rec {
      hedgehog = self.hedgehog_1_2;
      relude = overrideCabal super.relude (orig: { doCheck = false; });
      text = self.text_2_0_1;
      unordered-containers =
        overrideCabal super.unordered-containers (orig: { doCheck = false; });
    };

  elmFormatPackages = self: super:
    let
      aeson = self.aeson_2_1_1_0;
      text = self.text_2_0_1;
    in {
      elm-format-build = self.callCabal2nix "elm-format-build" ./Shakefile { };
      elm-format = self.callCabal2nix "elm-format" ./. { inherit aeson text; };
      avh4-lib = self.callCabal2nix "avh4-lib" ./avh4-lib { inherit text; };
      elm-format-lib = self.callCabal2nix "elm-format-lib" ./elm-format-lib {
        inherit aeson text;
      };
      elm-format-test-lib =
        self.callCabal2nix "elm-format-test-lib" ./elm-format-test-lib {
          inherit text;
          hspec-golden = self.hspec-golden_0_1_0_3;
        };
      elm-format-markdown =
        self.callCabal2nix "elm-format-markdown" ./elm-format-markdown {
          inherit text;
        };
    };

  pkgs = import nixpkgs {
    config = {
      packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
          packages = pkgs.haskell.packages // {
            project = pkgs.haskell.packages."${compiler}".override {
              overrides = haskellPackageOverrides pkgs;
            };
          };
        };
      };
    };
  };

  haskellPackages = pkgs.haskell.packages.project.extend elmFormatPackages;
in {
  elm-format = haskellPackages.elm-format;

  # Made available for debugging
  inherit haskellPackages pkgs;
}
