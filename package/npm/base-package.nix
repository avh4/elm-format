{
  stdenvNoCC,
  substituteAll,
  lib,
  ...
}: {
  name,
  version,
  prerelease ? null,
  npmScope,
  binaryPackages,
}: let
  concatLines = lib.concatMapStrings (s: s + "\n");

  npmPackageName =
    if npmScope == null
    then name
    else "@${npmScope}/${name}";

  npmVersion =
    if prerelease == null
    then version
    else "${version}-${prerelease}";

  package-json = let
    base = builtins.fromJSON (builtins.readFile ./package.json);
  in
    base
    // {
      name = npmPackageName;
      version = npmVersion;
      optionalDependencies = builtins.listToAttrs (lib.mapAttrsToList (key: p: {
          name = p.npmPackageName;
          value = p.npmPackageVersion;
        })
        binaryPackages);
    };

  packageFiles = package-json.files;

  copyStaticFile = file: ''
    cp ${./. + ("/" + file)} $out/${file}
  '';
in
  stdenvNoCC.mkDerivation {
    name = "${name}-${npmVersion}";

    phases = ["installPhase"];
    installPhase = ''
      mkdir -p $out
      cp ${builtins.toFile "package.json" (builtins.toJSON package-json)} $out/package.json
      mkdir -p $out/bin
      ${copyStaticFile "README.md"}
      ${concatLines (map copyStaticFile packageFiles)}
    '';
  }
