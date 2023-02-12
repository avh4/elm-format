{
  name = "elm-format-test";
  version = "0.8.5";
  prerelease = "rc.1";
  scope = "avh4";
  defaultBinaryScope = "avh4";
  experimental = false;
  elmVersions = [
    "0.18.0"
    "0.19.0"
    "0.19.1"
  ];

  binaries = {
    linux-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.5/elm-format-0.8.5-linux-x64.tgz";
      sha256 = "sha256-ZpUft1sRM5gJGDmAyrayIGmYRazoCJ/JFUtjWs/TKCU=";
    };
    mac-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.5/elm-format-0.8.5-mac-x64.tgz";
      sha256 = "sha256-gz1XExjxAwyHz+SxUzxxfXroxHUR/0Hp7l5jf7L2B44=";
    };
    mac-arm64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.5/elm-format-0.8.5-mac-x64.tgz";
      sha256 = "sha256-gz1XExjxAwyHz+SxUzxxfXroxHUR/0Hp7l5jf7L2B44=";
    };
    win-x64 = {
      v = "1";
      url = "https://github.com/avh4/elm-format/releases/download/0.8.5/elm-format-0.8.5-win-x64.zip";
      sha256 = "sha256-caZuv5qoPaPSDcvjDEpwXz1b+77Us7jzfhIWoVvPOrE=";
    };
  };
}
