var path = require("path");
var binstall = require(path.join(__dirname, "binstall/binstall.js"));
var url = require(path.join(__dirname, "elmFormatDownload.js")).url();

var binariesDir = path.join(__dirname, "binaries");
var packageInfo = require(path.join(__dirname, "package.json"));
var binaryExtension = process.platform === "win32" ? ".exe" : "";
var executablePaths = Object.keys(packageInfo.bin).map(function(executable) {
  return path.join(binariesDir, executable + binaryExtension);
});
var errorMessage = "Unfortunately, there are no elm-format " +
  packageInfo.version +
  " binaries available for your operating system and architecture.\n\nIf you would like to build elm-format from source, there are instructions at https://github.com/avh4/elm-format#building-from-source\n";

binstall(
  url,
  { path: binariesDir },
  { verbose: true, verify: executablePaths, errorMessage: errorMessage }
).then(
  function(successMessage) {
    console.log(successMessage);
  },
  function(errorMessage) {
    console.error(errorMessage);
    process.exit(1);
  }
);
