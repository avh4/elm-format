{ sources ? import ./nix/sources.nix { } }:
let
  project = import ./default.nix;
  pkgsNative = import sources.nixpkgs { };

  pkgsLinux = pkgsNative;
  pkgsMac = import sources.nixpkgs {
    crossSystem = pkgsNative.lib.systems.examples.x86_64-darwin;
  };
  pkgsWin = import sources.nixpkgs {
    crossSystem = pkgsNative.lib.systems.examples.mingw32;
  };

  pkgsTest = import sources.nixpkgs {
    crossSystem = pkgsNative.lib.systems.examples.aarch64-multiplatform;
  };

  linux = project { pkgs = pkgsLinux; };
  mac = project { pkgs = pkgsMac; };
  win = project { pkgs = pkgsWin; };
  test = project { pkgs = pkgsTest; };
in {
  elm-format-linux = linux.elm-format.components.exes.elm-format;
  #elm-format-mac = mac.elm-format.components.exes.elm-format;
  #elm-format-win = win.elm-format.components.exes.elm-format;
  elm-format-test = test.elm-format.components.exes.elm-format;
}
