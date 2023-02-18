{
  stdenvNoCC,
  substituteAll,
  lib,
  writeShellScript,
  ...
}: {
  name,
  version,
  prerelease ? null,
  npmScope,
  binaryPackageScope,
  binaryPackages,
  experimental,
  elmVersions,
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

  copyTemplateFile = file: let
    template = ./. + ("/" + file);
    rendered = substituteAll {
      src = template;
      inherit name npmPackageName binaryPackageScope;
    };
  in ''
    cp ${rendered} $out/${file}
  '';

  publish-sh = let
    distTag = tag: ''npm dist-tag add ${npmPackageName}@${npmVersion} ${tag}'';

    distTagElmVersion = elmVersion:
      distTag "latest-${elmVersion}";

    primaryTag =
      if prerelease != null
      then "rc"
      else if experimental
      then "exp"
      else "latest";

    extraPublishArgs =
      if npmScope == null
      then ""
      else "--access=public";
  in
    writeShellScript "publish.sh"
    (concatLines [
      ''
        set -euxo pipefail
        npm publish --tag ${primaryTag} ${extraPublishArgs}
      ''
      (
        if prerelease == null
        then ''
          ${distTag "exp"}
          ${concatLines (map distTagElmVersion elmVersions)}
        ''
        else ""
      )
    ]);
in
  stdenvNoCC.mkDerivation {
    name = "${name}-${npmVersion}";

    phases = ["installPhase"];
    installPhase = ''
      mkdir -p $out
      cp ${builtins.toFile "package.json" (builtins.toJSON package-json)} $out/package.json
      mkdir -p $out/bin
      ${copyTemplateFile "README.md"}
      ${concatLines (map copyTemplateFile packageFiles)}
      cp "${publish-sh}" "$out/publish.sh"
    '';
  }
