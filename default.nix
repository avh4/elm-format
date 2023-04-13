{ sources ? import ./nix/sources.nix, compiler ? "ghc944" }:

let
  gitignore = import sources."gitignore.nix" { };
  inherit (gitignore) gitignoreSource gitignoreFilterWith;

  haskellPackageOverrides = pkgs: self: super:
    with pkgs.haskell.lib; rec {
      hedgehog = self.hedgehog_1_2;
      relude = overrideCabal super.relude_1_2_0_0 (orig: { doCheck = false; });
      text = self.text_2_0_2;
      unordered-containers =
        overrideCabal super.unordered-containers (orig: { doCheck = false; });
    };

  elmFormatPackages = self: super:
    with pkgs.haskell.lib;
    let
      inherit (pkgs) lib;

      aeson = self.aeson_2_1_2_1;
      text = self.text_2_0_2;

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
      elm-format-test-lib =
        mkPkg "elm-format-test-lib" ./elm-format-test-lib { inherit text; };
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

  mkHaskellPackages = p:
    (p.haskell.packages.project.extend elmFormatPackages).extend (self: super:
      with pkgs.haskell.lib; {
        elm-format-static = justStaticExecutables super.elm-format;
      });

  # This is the haskell package set that includes elm-format and all its dependencies
  haskellPackages = mkHaskellPackages pkgs;

  # This is a haskell package set for the same ghc version that does not have our project package overries
  # (Because our override aren't compatible with haskell-language-server, etc)
  haskellTools = import ./nix/tools.nix { inherit sources compiler; };
in {
  elm-format = haskellPackages.elm-format;
  dist = {
    native = (mkHaskellPackages pkgs.pkgsStatic).elm-format-static;
    linux-x64 = (mkHaskellPackages pkgs.pkgsStatic).elm-format-static;
    linux-aarch64 = (mkHaskellPackages
      pkgs.pkgsCross.aarch64-multiplatform-musl.pkgsStatic).elm-format-static;
  };

  # Used by shell.nix
  inherit pkgs haskellPackages haskellTools;
}
