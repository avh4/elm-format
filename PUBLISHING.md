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

1. Edit `CHANGELOG.md` to set the correct version number.
1. Create `Release Notes/<version>.md` to draft the release notes.
1. Update the version number in `elm-format.cabal`.
1. If this is a stable release, update references to the version in `README.md`.
1. Update `ElmFormat.Version.experimental` to `Just <survey URL>` for experimental versions and `Nothing` otherwise.
1. `(cd package/npm && npm version "<new version>")`
1. Commit the changes "Bump version to *new version*"
1. Create a signed tag for the new version. `git tag -s <version> -m <version>`
1. Push the tag. `git push origin <version>`
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
1. Enter the contents of `Release Notes/<version>.md` as the release notes.
1. Upload the zip, tgz and asc files.
1. Publish the release.
1. Update `README.md`


## NPM

```
cd package/npm
npm install
# for experimental releases
# npm publish --tag exp
npm publish
npm dist-tag add elm-format@<new version> elm0.18.0
npm dist-tag add elm-format@<new version> elm0.19.0
npm dist-tag add elm-format@<new version> exp
```


## Nix

```
cd package/nix
./build.sh
```

Then `cd nixpkgs`, push the resulting branch, and make a PR to https://github.com/NixOS/nixpkgs with the title "elm-format: [old version] -> [new version]"
