var binstall = require("binstall");
var path = require("path");
var fs = require("fs");
var packageInfo = require(path.join(__dirname, "package.json"));

var binVersion = packageInfo.version;


// 'arm', 'ia32', or 'x64'.
var arch = process.arch;

// 'darwin', 'freebsd', 'linux', 'sunos' or 'win32'
var operatingSystem = process.platform;

var filename = operatingSystem + "-" + arch + ".tar.gz";
var url = "https://dl.bintray.com/elmlang/elm-format/default/"
  + binVersion + "/" + filename;

var binariesDir = path.join(__dirname, "binaries");
var packageInfo = require(path.join(__dirname, "package.json"));
var binaryExtension = process.platform === "win32" ? ".exe" : "";
var executablePaths = Object.keys(packageInfo.bin).map(function(executable) {
  return path.join(binariesDir, executable + binaryExtension);
});
var errorMessage = "Unfortunately, there are no elm-format " + binVersion + " binaries available on your operating system and architecture.\n\nIf you would like to build elm-format from source, there are instructions at https://github.com/avh4/elm-format#building-from-source\n";

binstall(url, {path: binariesDir},
  {verbose: true, verify: executablePaths, errorMessage: errorMessage}
).then(function(successMessage) {
    console.log(successMessage);
  }, function(errorMessage) {
    console.error(errorMessage);
    process.exit(1);
  });
