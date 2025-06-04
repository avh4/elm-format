{ sources ? import ./nix/sources.nix, compiler ? "ghc967" }:

let
  gitignore = import sources."gitignore.nix" { };
  inherit (gitignore) gitignoreFilterWith;

  haskellPackageOverrides = pkgs: self: super: { };

  elmFormatPackages = self: super:
    with pkgs.haskell.lib;
    let
      inherit (pkgs) lib;

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
      elm-format = mkPkg "elm-format" ./. { };
      avh4-lib = mkPkg "avh4-lib" ./avh4-lib { };
      elm-format-lib =
        mkPkg "elm-format-lib" ./elm-format-lib { };
      elm-format-test-lib =
        mkPkg "elm-format-test-lib" ./elm-format-test-lib { };
      elm-format-markdown =
        mkPkg "elm-format-markdown" ./elm-format-markdown { };
    };

  pkgs = import sources.nixpkgs {
    config = {
      packageOverrides = pkgs: {
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
