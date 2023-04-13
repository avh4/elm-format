{ sources ? import ./sources.nix, compiler }:

let
  haskellToolsOverrides = pkgs: self: super:
    with pkgs.haskell.lib; rec {
      # ghcid-0.8.8
      ghcid = self.callCabal2nix "ghcid" (builtins.fetchGit {
        url = "https://github.com/ndmitchell/ghcid.git";
        rev = "c38cdc97cd9470bb9025ea334d88d451bf1d8400";
      }) { fsnotify = self.fsnotify_0_4_1_0; };

      # string-qq's tests depend on text < 1.3
      string-qq = overrideCabal super.string-qq (orig: { doCheck = false; });
    };

  haskellToolsPkgs = import sources.nixpkgs {
    config = {
      packageOverrides = pkgs: rec {
        haskell = pkgs.haskell // {
          packages = pkgs.haskell.packages // {
            tools = pkgs.haskell.packages."${compiler}".override {
              overrides = haskellToolsOverrides pkgs;
            };
          };
        };
      };
    };
  };
in haskellToolsPkgs.haskell.packages.tools
