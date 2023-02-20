{ sources ? import ./nix/sources.nix, compiler ? "ghc925" }:

let
  gitignore = import sources."gitignore.nix" { };
  inherit (gitignore) gitignoreSource gitignoreFilterWith;

  haskellPackageOverrides = pkgs: self: super:
    with pkgs.haskell.lib; rec {
      hedgehog = self.hedgehog_1_2;
      relude = overrideCabal super.relude (orig: { doCheck = false; });
      text = self.text_2_0_1;
      unordered-containers =
        overrideCabal super.unordered-containers (orig: { doCheck = false; });
    };

  elmFormatPackages = self: super:
    with pkgs.haskell.lib;
    let
      inherit (pkgs) lib;

      aeson = self.aeson_2_1_1_0;
      text = self.text_2_0_1;

      mkPkg = name: path: args:
        overrideCabal (self.callCabal2nix name path args) (orig: {
          src = lib.cleanSourceWith {
            name = "source";
            filter = fpath: ftype:
              if builtins.elem fpath [
                (toString ./generated/Build_elm_format.hs)
                (toString ./generated)
              ] then
              # Include certain gitignored files
                true
              else if path == ./. && builtins.elem fpath [
                (toString ./avh4-lib)
                (toString ./elm-format-lib)
                (toString ./elm-format-test-lib)
                (toString ./elm-format-markdown)
              ] then
              # Exclude subprojects when building the top-level package
                false
              else
                gitignoreFilterWith { basePath = path; } fpath ftype;
            src = path;
          };
        });
    in {
      elm-format-build = mkPkg "elm-format-build" ./Shakefile { };
      elm-format = mkPkg "elm-format" ./. { inherit aeson text; };
      avh4-lib = mkPkg "avh4-lib" ./avh4-lib { inherit text; };
      elm-format-lib =
        mkPkg "elm-format-lib" ./elm-format-lib { inherit aeson text; };
      elm-format-test-lib = mkPkg "elm-format-test-lib" ./elm-format-test-lib {
        inherit text;
        hspec-golden = self.hspec-golden_0_1_0_3;
      };
      elm-format-markdown =
        mkPkg "elm-format-markdown" ./elm-format-markdown { inherit text; };
    };

  pkgs = import sources.nixpkgs {
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
