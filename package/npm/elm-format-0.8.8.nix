{
  name = "elm-format";
  version = "0.8.8";
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
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.8/elm-format-0.8.8-linux-x64.tgz";
      sha256 = "sha256-InbNKkpQnmmHQYE7oZ4mf8YCvg5jYK2uK0XcnRqHGhw=";
    };
    linux-aarch64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.8/elm-format-0.8.8-linux-aarch64.tgz";
      sha256 = "sha256-STfNmMG+yh2wXhFGFuxDmVpWY5J5xSw+l0s0JtdSkZ0=";
    };
    mac-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.8/elm-format-0.8.8-mac-x64.tgz";
      sha256 = "sha256-HNgaT/nveUklH6MK209iGeZhKbvU7p7/l/6MDp18r10=";
    };
    mac-arm64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.8/elm-format-0.8.8-mac-arm64.tgz";
      sha256 = "sha256-U5/fLt+zxJbCaCNSrXM9kR+QYh4+kUjJS9Hhcuxlq3w=";
    };
    win-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.8/elm-format-0.8.8-win-x64.zip";
      sha256 = "sha256-2ssJQEWDVozdlTZZFqJimfOJV3bmHgw93iOorM9guc4=";
    };
  };
}
