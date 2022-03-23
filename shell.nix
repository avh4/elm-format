{ sources ? import nix/sources.nix, pkgs ? import sources.nixpkgs { } }:
let
  project = import ./default.nix { sources = sources; };
  niv = import sources.niv { };
  haskellNix = import sources.haskellNix { };
in project.shellFor {
  # packages = p: [ ];

  tools = { cabal = "latest"; };
  #exactDeps = true;

  buildInputs = with pkgs; [
    # Tools required to build elm-format
    git
    python3
    jq

    # Dev tools
    ghcid
    cabal2nix
    niv.niv
    nixfmt
    haskellNix.pkgs.haskell-nix.nix-tools.ghc901
  ];
}
