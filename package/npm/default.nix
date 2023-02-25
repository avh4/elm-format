{pkgs ? import <nixpkgs> {}}:
import ./workspace.nix pkgs (import ./elm-format-0.8.6-rc.1.nix)
