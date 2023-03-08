args@{ ... }:
let
  default = import ./. args;
  inherit (default) pkgs haskellPackages haskellTools;
in haskellPackages.shellFor {
  name = "elm-format";
  packages = p:
    with p; [
      elm-format-build
      elm-format
      avh4-lib
      elm-format-lib
      elm-format-test-lib
      elm-format-markdown
    ];
  buildInputs = with pkgs; [
    # Tools required to build elm-format
    cabal-install
    git
    python3
    jq

    # Dev tools
    hpack
    haskellTools.ghcid
    cabal2nix
    niv
    nixfmt
    alejandra
    haskellTools.haskell-language-server
    unzip
    nix-prefetch
  ];
}
