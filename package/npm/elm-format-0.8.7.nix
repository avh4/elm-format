{
  name = "elm-format";
  version = "0.8.7";
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
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-linux-x64.tgz";
      sha256 = "sha256-4iIRtg4j1DjlVG8q/VLrEKDnR2CuUR1iE0J/IVOj/G0=";
    };
    linux-aarch64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-linux-aarch64.tgz";
      sha256 = "sha256-jMvrk4AfyX/0Mzh/QZE605PZKnCLAYFVtmN4AprH+08=";
    };
    mac-x64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-mac-x64.tgz";
      sha256 = "sha256-2Fz2jVFeUUz+c0laE/IvJSlQdDDIibOwHu82r/oXAhA=";
    };
    mac-arm64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-mac-arm64.tgz";
      sha256 = "sha256-OqGWT5ybgon4xtFMpaz2MGtMSHTbHf7odhC3Ef9mh6E=";
    };
    win-x64 = {
      v = "2";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.7/elm-format-0.8.7-win-x64.zip";
      sha256 = "sha256-WZgJKDwGQhNcb8YvZn2AlmyxbtlHOdf3P22dv4ZdaxI=";
    };
  };
}
