# How to publish releases of elm-format


## Workstation setup

You will need an appropriate GPG signing key created and set as the default key.


## Preparation

1. Start a new branch from `origin/main` named `release/<new version>`
1. Edit `CHANGELOG.md` to set the correct version number.
1. Create `Release Notes/<new version>.md` to draft the release notes.
1. Update the version number in the top-level `package.yaml`, and run `dev/build.sh`.
1. If this is a stable release, update references to the version in `README.md`.
1. In `src/ElmFormat/Version.hs`, update `ElmFormat.Version.experimental` to `Just <survey URL>` for experimental versions and `Nothing` otherwise.
1. Commit the changes "Bump version to \<new version>"
1. Create a signed tag for the new version. `git tag -s <new version> -m <new version>`
1. Push the tag. `git push origin <new version>`
1. Wait for CI to successfully build the tag.


## Platforms built on CI (Mac / Windows)

1. Pushing the tag should have triggered a builds at:
    - <https://github.com/avh4/elm-format/actions/workflows/Build%20release.yml>
    - <https://github.com/avh4/elm-format/actions/workflows/Build%20release%20(lamdera-community).yml>
1. Download the artifacts from the successful builds (keep them as zip files), and put them in `downloads/`


## Platforms built via nix (Linux)

1. Make sure you have [Nix](https://nixos.org/) installed
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
./build.sh
```

Then `cd elm-tooling`, push the resulting branch, and make a PR to <https://github.com/elm-tooling/elm-tooling-cli> with the title "`Add elm-format <new version>`"


## NPM

1. `cd package/npm`
1. Create `elm-format-<new-version>.nix`:
    - Fill out the release info
        - `prerelease` is null for normal releases, or should have the format `(alpha|beta|rc).[0-9]+` for prereleases
        - `scope` is optional and will create a scoped top-level package (don't set it for normal releases)
        - `binaryPackageScope` is the scope that the binary packages will be published under
    - Fill out the `binaries` info for each supported platform
        - To get the sha256, run `nix-prefetch fetchzip --url <release url>`
        - `v` is the sub-patch version for this binary and can be incremented as needed (but should be reset to `"1"` on each new elm-format version)
1. Update `default.nix` to point to the new `elm-format-<new-version>.nix`
1. `nix-shell --pure`
1. For each folder in `./elm-format-*-*`, go to the folder and `./publish.sh`
1. For the main package, `cd elm-format` and `./publish.sh`


## Nix

You must checkout the exact commit that is tagged for the release (so that `git describe` prints only the tag name),
and then run:

```sh
cd package/nix
./build.sh
```

Then `cd nixpkgs`, push the resulting branch, and make a PR to <https://github.com/NixOS/nixpkgs> with the title "`elm-format: <old version> -> <new version>`"


## Cleanup

1. Create and merge a PR for the `release/<new version>` branch
1. Delete the `release/<new version>` branch
