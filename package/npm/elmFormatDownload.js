var path = require("path");

exports.url = function(operatingSystem, arch) {
  // 'arm', 'ia32', or 'x64'.
  arch = arch || process.arch;

  // 'darwin', 'freebsd', 'linux', 'sunos' or 'win32'
  operatingSystem = operatingSystem || process.platform;

  var packageInfo = require(path.join(__dirname, "package.json"));
  var binVersion = packageInfo.version;

  var filename = operatingSystem + "-" + arch + ".tar.gz";
  var url = "https://dl.bintray.com/elmlang/elm-format/default/" +
    binVersion +
    "/" +
    filename;

  return url;
};
