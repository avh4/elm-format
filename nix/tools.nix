{ sources ? import ./sources.nix, compiler }:

let
  haskellToolsOverrides = pkgs: self: super: { };

  haskellToolsPkgs = import sources.nixpkgs {
    config = {
      packageOverrides = pkgs: {
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
