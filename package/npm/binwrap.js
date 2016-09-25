const path = require('path');
const spawn = require('child_process').spawn;

const distDir = require('./config').distDir;

module.exports = function(executable) {
  var input = process.argv.slice(2);

  const os = process.platform;

  // Unzipping the Windows .zip puts the .exe inside the directory,
  // while the .tgz will produce just the right binary files with
  // the same name as the executable. path.join will ignore empty strings.
  const binaryFile = os === 'win32' ? 'elm-format.exe' : '';

  spawn(path.join(distDir, executable, binaryFile), input, { stdio: 'inherit' })
    .on('exit', process.exit);
};
