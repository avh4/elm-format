{pkgs ? import <nixpkgs> {}}: let
  workspace = import ./. {inherit pkgs;};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [nodejs];
    shellHook = ''
      cd ${workspace}
      export PS1="[Publishing elm-format] "
    '';
  }
