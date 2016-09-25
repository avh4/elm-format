const fs = require('fs');
const zlib = require('zlib');
const request = require('request');
const tar = require('tar');
const unzip = require('unzip');
const mkdirp = require('mkdirp');

const distDir = require('./config').distDir;

const elmFormatVersion = require('./config').elmFormatVersion;

getDownloadUrls()
  .then(downloadAndUnpackBinaries)
  .then(successMessages => {
    successMessages.forEach(message => {
      console.log(message);
    });
  })
  .catch(errorMessage => {
    console.error(errorMessage);
    process.exit(1);
  });

function getDownloadUrls() {
  return new Promise((resolve, reject) => {
    const availableOs = {
      darwin: 'mac',
      linux: 'linux',
      win32: 'win',
    };

    const availableArch = {
      x64: 'x64',
    };

    const os = availableOs[process.platform];
    const arch = availableArch[process.arch];

    if (!os || !arch) {
      return reject('Sorry, there are currently no available elm-format binary for your operating system and architecture.');
    }

    const extension = os === 'win' ? 'zip' : 'tgz';

    const elmVersions = [
      '0.16',
      '0.17',
    ];

    return resolve(
      elmVersions.map(version => ({
        version,
        filename: versionToFilename(version),
        url: filenameToDownloadUrl(versionToFilename(version))
      }))
    );

    function versionToFilename(version) {
      return `elm-format-${version}-${elmFormatVersion}-${os}-${arch}.${extension}`;
    }

    function filenameToDownloadUrl(filename) {
      return `https://github.com/avh4/elm-format/releases/download/${elmFormatVersion}/${filename}`
    }
  })
}

function downloadAndUnpackBinaries(downloadObjects) {
  console.log('Downloading binaries...');
  return Promise.all(downloadObjects.map(downloadAndUnpack));
}

function downloadAndUnpack(downloadObject) {
  return new Promise((resolve, reject) => {
    const gunzip = zlib.createGunzip()
      .on('error', (err) => {
        reject(`Error decompressing ${downloadObject.filename} ${err}`);
      });

    const untar = tar.Extract({ path: `${distDir}/elm-format-${downloadObject.version}`, strip: 1 })
      .on('error', (err) => {
        reject(`Error decompressing ${downloadObject.filename} ${err}`);
      })
      .on('end', onEnd);

    const extractZip = unzip.Extract({ path: `${distDir}/elm-format-${downloadObject.version}` })
      .on('end', onEnd);

    const req = request.get(downloadObject.url, (err, response) => {
      if (err) {
        return reject(`Error communicating with URL ${downloadObject.url}`);
      }

      if (response.statusCode !== 200) {
        return reject(`Error: URL is returning status code other than 200: ${response.statusCode}`);
      }

      if (!fs.existsSync(distDir)) {
        mkdirp.sync(distDir);
      }

      response.on('error', (err) => {
        reject(`Error receiving ${downloadObject.url} ${err}`);
      });
    });

    if (process.platform === 'win32') {
      req.pipe(extractZip);
    } else {
      req.pipe(gunzip).pipe(untar);
    }

    function onEnd() {
      if (!fs.existsSync(distDir)) {
        reject(
          'Error extracting executables: extraction finished, but',
          distDir, 'directory was not created.\n' +
          'Current directory contents: ' + fs.readdirSync(__dirname)
        );
      }

      resolve('Successfully downloaded and processed ' + downloadObject.filename);
    }
  });
}