{pkgs ? import <nixpkgs> {}}: let
  workspace = import ./. {inherit pkgs;};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      # Required
      nodejs

      # Debugging tools
      jq
      jless
    ];
    shellHook = ''
      cd ${workspace}
      export PS1="[Publishing elm-format] "
    '';
  }
