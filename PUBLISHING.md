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

1. Run `dev/build.sh publish-<new version>`
1. Go to the release page for the new tag on github.
1. Enter the contents of `Release Notes/<new version>.md` as the release notes.
1. Upload the zip, tgz and asc files.
1. Publish the release.


## elm-tooling

```sh
cd package/elm-tooling
dev/build.sh
```

Then `cd elm-tooling`, push the resulting branch, and make a PR to <https://github.com/elm-tooling/elm-tooling-cli> with the title "`Add elm-format <new version>`"


## NPM

1. `cd package/npm`
1. Create `elm-format-<new-version>.nix`:
    - Fill out the release info
        - `prerelease` is optional and should have the format `(alpha|beta|rc).[0-9]+` if present
        - `scope` is optional and will create a scoped top-level package (don't set it for normal releases)
        - `binaryPackageScope` is the scope that the binary packages will be published under
    - Fill out the `binaries` info for each supported platform
        - To get the sha256, run `nix-prefetch fetchzip --url <release url>`
        - `v` is the sub-patch version for this binary and can be incremented as needed (but should be reset to `"1"` on each new elm-format version)
1. `nix-shell --pure`
1. For each folder in `./elm-format-*-*`, go to the folder and `./publish.sh`

Then publish the main npm package:

```sh
cd elm-format
npm install
./publish.sh
```


## Nix

```sh
cd package/nix
dev/build.sh
```

Then `cd nixpkgs`, push the resulting branch, and make a PR to <https://github.com/NixOS/nixpkgs> with the title "`elm-format: <old version> -> <new version>`"


## Cleanup

1. Create and merge a PR for the `release/<new version>` branch
1. Delete the `release/<new version>` branch
