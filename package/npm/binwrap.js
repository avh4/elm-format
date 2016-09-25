const path = require('path');
const spawn = require('child_process').spawn;

const distDir = require('./config').distDir;

module.exports = function(executable) {
  var input = process.argv.slice(2);

  const os = process.platform;

  const extension = os === 'win32' ? '.exe' : '';
  const filename = `${executable}${extension}`;

  spawn(path.join(distDir, filename), input, { stdio: 'inherit' })
    .on('exit', process.exit);
};
