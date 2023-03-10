{
  name = "elm-format";
  version = "0.8.6";
  prerelease = null;
  binaryPackageScope = "avh4";
  experimental = false;
  elmVersions = [
    "0.18.0"
    "0.19.0"
    "0.19.1"
  ];

  binaries = {
    linux-x64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6/elm-format-0.8.6-linux-x64.tgz";
      sha256 = "sha256-gxTs7ZDr9S2td3BL6e+xNagMXAOg0vfwHyR6fT4QbHk=";
    };
    linux-aarch64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6/elm-format-0.8.6-linux-aarch64.tgz";
      sha256 = "sha256-QTXAjqwmfCyVau87aX6V/iEm6YazAM3VNzUGfUrUBgI=";
    };
    mac-x64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6/elm-format-0.8.6-mac-x64.tgz";
      sha256 = "sha256-0Naa8n6o5NxBUMetNkMDrnaE6wN64vrzBYrct4gECRk=";
    };
    mac-arm64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6/elm-format-0.8.6-mac-arm64.tgz";
      sha256 = "sha256-DG0fY5lti8JpQQ0wdby3PZeJhFUPJLBjDdKBX5OGGrU=";
    };
    win-x64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6/elm-format-0.8.6-win-x64.zip";
      sha256 = "sha256-FlXsAoKhSSfgEK8zFxbR1z3rgo+mJYolAi4rNLwVmkE=";
    };
  };
}
