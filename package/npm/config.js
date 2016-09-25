const path = require('path');

const packageInfo = require(path.join(__dirname, 'package.json'));

module.exports = {
  distDir: path.join(__dirname, 'bin'),
  elmFormatVersion: packageInfo.version,
};
