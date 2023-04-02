{
  name = "elm-format";
  version = "0.8.7";
  prerelease = "rc.1";
  binaryPackageScope = "avh4";
  experimental = false;
  elmVersions = [
    "0.18.0"
    "0.19.0"
    "0.19.1"
  ];

  binaries = {
    linux-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7-rc.1/elm-format-0.8.7-rc.1-linux-x64.tgz";
      sha256 = "sha256-yMD57UqAAh9NzVL+7eZNLscvD1qnJ8CG5EqrJ1i6z78=";
    };
    linux-aarch64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7-rc.1/elm-format-0.8.7-rc.1-linux-aarch64.tgz";
      sha256 = "sha256-fPEDXsK99vZbrf248f5uuEjzZK2a06GR34UBzw0WDvs=";
    };
    mac-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7-rc.1/elm-format-0.8.7-rc.1-mac-x64.tgz";
      sha256 = "sha256-itMJlqommJJzchcYwUnQpci5KPRcctfzm+DKyKBzsp0=";
    };
    mac-arm64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7-rc.1/elm-format-0.8.7-rc.1-mac-arm64.tgz";
      sha256 = "sha256-kdMTab6hR9mGN4pWoAztkhl4mflwO0K8U0BV+gbM7GM=";
    };
    win-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7-rc.1/elm-format-0.8.7-rc.1-win-x64.zip";
      sha256 = "sha256-VXmZ5CU7HJpihKT+M7kRup8S+f3MtXu4w5dfkQlzOyM=";
    };
  };
}
