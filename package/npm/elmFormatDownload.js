var path = require("path");

exports.url = function(operatingSystem, arch) {
  // 'arm', 'ia32', or 'x64'.
  arch = arch || process.arch;
  arch = {}[arch] || arch;

  // 'darwin', 'freebsd', 'linux', 'sunos' or 'win32'
  operatingSystem = operatingSystem || process.platform;
  operatingSystem = {
    darwin: "mac",
    win32: "win"
  }[operatingSystem] || operatingSystem;

  if (operatingSystem === "win") {
    arch = "i386";
  }

  var ext = {
    win: "zip"
  }[operatingSystem] || "tgz";

  var packageInfo = require(path.join(__dirname, "package.json"));
  var binVersion = packageInfo.version.split("+")[0];

  var url = "https://github.com/avh4/elm-format/releases/download/" +
    binVersion +
    "/elm-format-0.18-" +
    binVersion +
    "-" +
    operatingSystem +
    "-" +
    arch +
    "." +
    ext;

  return url;
};
