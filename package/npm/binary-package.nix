{
  stdenvNoCC,
  writeShellScript,
  ...
}: {
  name,
  platform,
  baseVersion,
  binaryVersion,
  npmScope,
  projectUrl ? "https://github.com/avh4/elm-format",
  binarySrc,
}: let
  platformInfo = (import ./platform-info.nix)."${platform}";
  inherit (platformInfo) binExt;
  npmPlatformString = with platformInfo.npm; "${os}-${cpu}";

  packageName = "${name}-${npmPlatformString}";
  npmPackageName = "@${npmScope}/${packageName}";
  npmPackageVersion = "${baseVersion}-${binaryVersion}";

  package-json = builtins.toFile "package.json" (builtins.toJSON {
    name = npmPackageName;
    version = npmPackageVersion;
    description = "The ${platform} binary for ${name}.";
    repository = projectUrl;
    license = "BSD-3-Clause";
    os = [platformInfo.npm.os];
    cpu = [platformInfo.npm.cpu];
    files = ["elm-format${binExt}"];
  });

  readme = builtins.toFile "README.md" ''
    This is the ${platform} binary for [${name}](${projectUrl}).
  '';

  publish-sh =
    writeShellScript "publish.sh"
    ''
      set -euxo pipefail
      npm publish --access=public
    '';
in
  stdenvNoCC.mkDerivation {
    name = "${packageName}-${npmPackageVersion}";

    phases = ["unpackPhase" "installPhase"];
    src = binarySrc;
    installPhase = ''
      mkdir -p $out
      cp "${package-json}" "$out/package.json"
      cp "${readme}" "$out/README.md"
      cp -L "elm-format${binExt}" "$out/elm-format${binExt}"
      chmod +x "$out/elm-format${binExt}"
      cp "${publish-sh}" "$out/publish.sh"
    '';
  }
  // {
    inherit npmPackageName npmPackageVersion;
    npmUnscopedPackageName = packageName;
  }
