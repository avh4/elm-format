[![Build Status](https://travis-ci.org/avh4/elm-format.svg?branch=master)](https://travis-ci.org/avh4/elm-format)
![experimental](https://img.shields.io/badge/stability-experimental-orange.svg)

# elm-format

`elm-format` is the official source code formatter for Elm.


## Basic Usage

```bash
elm-format Main.elm
```


## Development info

### Building from source

```bash
git clone https://github.com/avh4/elm-format.git
cd elm-format
cabal sandbox init --sandbox=.cabal-sandbox
cabal install -j
.cabal-sandbox/bin/elm-format --help
```

### Running tests

After installing with the instructions above:

```bash
./tests/run-tests.sh
```
