var binwrap = require("binwrap");
var path = require("path");

var packageInfo = require(path.join(__dirname, "package.json"));
var binVersion = packageInfo.version;

var root = "https://github.com/avh4/elm-format/releases/download/" +
  binVersion +
  "/elm-format-" +
  binVersion;

module.exports = binwrap({
  dirname: __dirname,
  binaries: ["elm-format"],
  urls: {
    "darwin-x64": root + "-mac-x64.tgz",
    "darwin-arm64": root + "-mac-x64.tgz",
    "linux-x64": root + "-linux-x64.tgz",
    "win32-x64": root + "-win-x64.zip"
  }
});
