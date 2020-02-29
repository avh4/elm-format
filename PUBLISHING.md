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

1. Start a new branch from `origin/master` named `release/<new version>`
1. Edit `CHANGELOG.md` to set the correct version number.
1. Create `Release Notes/<new version>.md` to draft the release notes.
1. Update the version number in `elm-format.cabal`.
1. If this is a stable release, update references to the version in `README.md`.
1. Update `ElmFormat.Version.experimental` to `Just <survey URL>` for experimental versions and `Nothing` otherwise.
1. `(cd package/npm && npm version "<new version>")`
1. Commit the changes "Bump version to <new version>"
1. Create a signed tag for the new version. `git tag -s <new version> -m <new version>`
1. Push the tag. `git push origin <new version>`
1. Wait for CI to successfully build the tag.


## Mac

1. Run `./package/mac/build-package.sh`


## Linux

1. Make sure you have Docker installed and running
1. Run `./package/linux/build-in-docker.sh`


## Windows

1. Pushing the tag should have triggered a build at <https://github.com/avh4/elm-format/actions?query=workflow%3A%22Build+Windows+release%22>
1. Download the zip file artifact from the successful build (github will wrap this in another zip file)
1. Unzip the outer zip file to get the inner zip file
1. Rename the inner zip file to `elm-format-<new version>-win-i386.zip`
1. Check that the SHA1 hash of the zip file matches what was printed in the "Run Get-FileHash -Algorithm SHA1 elm-format.zip" step of the successful build


## Publishing

1. Run `./package/sign_files.sh`
1. Go to the release page for the new tag on github.
1. Enter the contents of `Release Notes/<new version>.md` as the release notes.
1. Upload the zip, tgz and asc files.
1. Publish the release.


## NPM

```
cd package/npm
npm install
# for experimental releases
# npm publish --tag exp
npm publish
npm dist-tag add elm-format@<new version> exp
npm dist-tag add elm-format@<new version> latest-0.18.0
npm dist-tag add elm-format@<new version> latest-0.19.0
npm dist-tag add elm-format@<new version> latest-0.19.1
```


## Nix

```
cd package/nix
./build.sh
```

Then `cd nixpkgs`, push the resulting branch, and make a PR to <https://github.com/NixOS/nixpkgs> with the title "`elm-format: <old version> -> <new version>`"


## Cleanup

1. Create and merge a PR for the `release/<new version>` branch
1. Delete the `release/<new version>` branch
