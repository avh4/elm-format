# How to publish releases of elm-format


## Preparation

1. Create a github issue to draft the release notes.
1. Edit `elm-format.cabal` to remove `-dev` from the version and make sure the version number is correct.
1. Commit the change to `elm-format.cabal`.
1. Create the tag for the new version.
1. Push the tag.


## Mac

1. Run `./build-package.sh`


## Windows

1. See comments in `./build-package.sh`


## Linux

1. See comments in `./build-package.sh`


## Publishing

1. Go to the release page for the new tag on github.
1. Upload the zip, tgz and asc files.
1. Write the release notes.
1. Publish the release.
1. Update `README.md`


## Clean up

1. Edit `elm-format.cabal` to increment the patch version number and add `-dev`.
1. Commit the change to `elm-format.cabal`.
