npm package for elm-format
==========================

This package installs [`elm-format`](https://github.com/avh4/elm-format) via [`npm`](https://www.npmjs.com/).

## Usage

```
npm install -g elm-format@0.4.0-alpha
```

This will install the binaries `elm-format` and `elm-format-0.16` to your global `node_modules`.

## Publishing

Publishing npm packages follows the [procedure stated in the official npm docs](https://docs.npmjs.com/getting-started/publishing-npm-packages).

### Bumping `elm-format` version

The downloaded binary version will follow the one stated in `package.json`. If `elm format` is updated, this package can be bumped by only updating the `version` field in the `package.json` (given that the download url structure does not change).
