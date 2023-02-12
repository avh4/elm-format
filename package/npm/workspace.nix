{
  stdenvNoCC,
  lib,
  fetchzip,
  ...
} @ pkgs: releaseConfig: let
  name = releaseConfig.name;
  version = releaseConfig.version;

  concatLines = lib.concatMapStrings (s: s + "\n");

  mkBinaryPackage = import ./binary-package.nix pkgs;

  binaryPackages = builtins.listToAttrs (lib.mapAttrsToList (platform: src: {
      name = "${name}-${platform}";
      value = mkBinaryPackage {
        inherit name platform;
        baseVersion = version;
        binaryVersion = src.v;
        npmScope = src.scope or releaseConfig.defaultBinaryScope;
        binarySrc = fetchzip {
          inherit (src) url sha256;
          inherit name version;
        };
      };
    })
    releaseConfig.binaries);

  basePackage = import ./base-package.nix pkgs {
    inherit (releaseConfig) name version prerelease elmVersions experimental;
    npmScope = releaseConfig.scope or null;
    inherit binaryPackages;
  };

  linkPackage = name: value: ''
    ln -s ${value} $out/${name}
  '';
in
  stdenvNoCC.mkDerivation {
    name = "${name}-npm-workspace-${version}";

    phases = ["installPhase"];
    installPhase = ''
      mkdir -p $out
      ${concatLines (lib.mapAttrsToList linkPackage binaryPackages)}
      ${linkPackage name basePackage}
    '';
  }
