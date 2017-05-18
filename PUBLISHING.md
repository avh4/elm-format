# How to publish releases of elm-format


## Workstation setup

### Mac

```bash
brew update
brew install keybase
brew install github-release
brew install caskroom/cask/brew-cask
brew cask install vagrant
brew cask install virtualbox
```


## Preparation

1. Create a github issue to draft the release notes.
1. Edit `elm-format.cabal` to remove `-dev` from the version and make sure the version number is correct.
1. Edit `CHANGELOG.md` to set the correct version number.
1. Update `ElmFormat.Version.experimental` to `Just <survey URL>` for experimental versions and `Nothing` otherwise.
1. `(cd package/npm && npm version "<new version>")`
1. Commit the changes "Bump version to *new version*"
1. Create a signed tag for the new version. `git tag -s <version> -m <version>`
1. Push the tag.
1. Wait for CI to successfully build the tag.


## Mac

1. Run `./package/mac/build-package.sh`


## Linux

1. Make sure you have Docker installed and running
1. Run `./package/linux/build-in-docker.sh`


## Windows

1. See `package/win/setup.md`


## Publishing

1. Run `package/collect_files.sh`
1. Go to the release page for the new tag on github.
1. Upload the zip, tgz and asc files.
1. Write the release notes.
1. Publish the release.
1. Update `README.md`


## NPM

```
cd package/npm
# for experimental releases
# npm publish --tag exp
npm publish
```


## Clean up

1. Edit `elm-format.cabal` with the next minor version number and add `-dev`.
1. Commit the change to `elm-format.cabal`.
