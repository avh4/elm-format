{
  description = "elm-format - A source code formatter for Elm (with teleport-imports)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.haskell.lib.compose;

        # Derive a version string from the flake source
        version = "0.8.7";
        gitRev = self.shortRev or self.dirtyShortRev or "unknown";

        # Use GHC 9.4 to match cabal.project's with-compiler: ghc-9.4.4
        haskellPackages = pkgs.haskell.packages.ghc94.override {
          overrides = hself: hsuper: {
            avh4-lib =
              lib.doJailbreak (hself.callCabal2nix "avh4-lib" ./avh4-lib {});
            elm-format-markdown =
              lib.doJailbreak (hself.callCabal2nix "elm-format-markdown" ./elm-format-markdown {});
            elm-format-lib =
              lib.doJailbreak (hself.callCabal2nix "elm-format-lib" ./elm-format-lib {});
            elm-format-test-lib =
              lib.doJailbreak (hself.callCabal2nix "elm-format-test-lib" ./elm-format-test-lib {});
            elm-format =
              lib.justStaticExecutables
                (lib.overrideCabal (drv: {
                  preBuild = (drv.preBuild or "") + ''
                    mkdir -p generated
                    cat > generated/Build_elm_format.hs << 'HSEOF'
                    module Build_elm_format where

                    gitDescribe :: String
                    gitDescribe = "${version}-${gitRev}"
                    HSEOF
                  '';
                })
                (lib.doJailbreak (hself.callCabal2nix "elm-format" ./. {})));
          };
        };
      in
      {
        packages.default = haskellPackages.elm-format;
        packages.elm-format = haskellPackages.elm-format;

        apps.default = {
          type = "app";
          program = "${haskellPackages.elm-format}/bin/elm-format";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [
            p.avh4-lib
            p.elm-format-markdown
            p.elm-format-lib
            p.elm-format-test-lib
            p.elm-format
          ];
          buildInputs = [
            pkgs.cabal-install
            haskellPackages.ghc
          ];
        };
      });
}
