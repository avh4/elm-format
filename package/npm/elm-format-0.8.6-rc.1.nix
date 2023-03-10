{
  name = "elm-format";
  version = "0.8.6";
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
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6-rc.1/elm-format-0.8.6-rc.1-linux-x64.tgz";
      sha256 = "sha256-NzJBqNgEWeMtAXI+wtlRe9u8KqXkrYg4CoYR5P/tuHc=";
    };
    linux-aarch64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6-rc.1/elm-format-0.8.6-rc.1-linux-aarch64.tgz";
      sha256 = "sha256-pZN0wu92+axxkAY0u+aUIHk1dJWYuZuYjxE7iy+bj+k=";
    };
    mac-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6-rc.1/elm-format-0.8.6-rc.1-mac-x64.tgz";
      sha256 = "sha256-Ua2AcqaWb21pdoL8oeFjVhFsFH0r8CakNMlXZiUX1T8=";
    };
    mac-arm64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6-rc.1/elm-format-0.8.6-rc.1-mac-arm64.tgz";
      sha256 = "sha256-CAoI8O4sqrPLH1SLWhTIOnDip0IdQjJsEmzomFi+Zao=";
    };
    win-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.6-rc.1/elm-format-0.8.6-rc.1-win-x64.zip";
      sha256 = "sha256-3m/0RZtjhQq6SWVjsjTTXPALnkTXYRG5fOC5zBC39eg=";
    };
  };
}
