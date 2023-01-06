# How to publish releases of elm-format


## Workstation setup

### Mac

```bash
brew update
brew install keybase
```


## Preparation

1. Start a new branch from `origin/main` named `release/<new version>`
1. Edit `CHANGELOG.md` to set the correct version number.
1. Create `Release Notes/<new version>.md` to draft the release notes.
1. Update the version number in `elm-format.cabal`.
1. If this is a stable release, update references to the version in `README.md`.
1. Update `ElmFormat.Version.experimental` to `Just <survey URL>` for experimental versions and `Nothing` otherwise.
1. `(cd package/npm && npm version "<new version>")`
1. Commit the changes "Bump version to \<new version>"
1. Create a signed tag for the new version. `git tag -s <new version> -m <new version>`
1. Push the tag. `git push origin <new version>`
1. Wait for CI to successfully build the tag.


## Platforms built on CI (Mac / Windows)

1. Pushing the tag should have triggered a build at <https://github.com/avh4/elm-format/actions?query=workflow%3A%22Build+release%22>
1. Download the artifacts from the successful build (keep them as zip files), and put them in `downloads/`


## Platforms built in docker (Linux)

1. Make sure you have [Docker](https://docs.docker.com/get-docker/) installed and running
1. The binaries will be built automatically in the next section


## Publishing

1. Run `./build.sh publish-<new version>`
1. Go to the release page for the new tag on github.
1. Enter the contents of `Release Notes/<new version>.md` as the release notes.
1. Upload the zip, tgz and asc files.
1. Publish the release.


## elm-tooling

```sh
cd package/elm-tooling
./build.sh
```

Then `cd elm-tooling`, push the resulting branch, and make a PR to <https://github.com/elm-tooling/elm-tooling-cli> with the title "`Add elm-format <new version>`"


## NPM

Repeat this for all the binary packages in `package/npm/packages/`. This uses `package/npm/packages/elm-format-darwin-x64` as an example.

1. Go to the folder: `cd package/npm/packages/elm-format-darwin-x64`
1. Copy the appropriate binary to `./elm-format`. For Windows: `./elm-format.exe`
1. Double-check that you put the right binary in the right package: `file elm-format`
1. Double-check that the file is executable: `ls -l elm-format`
1. In `package.json` of the binary package, bump the version for example to `"0.8.6"`.
1. In `package.json` of the main npm package, update `"optionalDependencies"` to point to the bumped version. For example: `"@avh4/elm-format-darwin-x64": "0.8.6"`.

   Note: Pin the versions of the binary packages _exactly_ – no version ranges. This means that installing `elm-format@0.8.6` installs the exact same bytes in two years as today.
1. Publish the package: `npm publish --access=public`

   `--access=public` is needed because scoped packages are private by default.

Then publish the main npm package:

```sh
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

```sh
cd package/nix
./build.sh
```

Then `cd nixpkgs`, push the resulting branch, and make a PR to <https://github.com/NixOS/nixpkgs> with the title "`elm-format: <old version> -> <new version>`"


## Cleanup

1. Create and merge a PR for the `release/<new version>` branch
1. Delete the `release/<new version>` branch
