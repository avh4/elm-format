[![Build Status](https://travis-ci.org/avh4/elm-format.svg?branch=master)](https://travis-ci.org/avh4/elm-format)
![experimental](https://img.shields.io/badge/stability-experimental-orange.svg)

# elm-format

`elm-format` is the official source code formatter for Elm.

`elm-format` is still in alpha.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues/new).


## Basic Usage

```bash
elm-format Main.elm
```

## Installation (0.1-alpha2)

`elm-format` is still in alpha.  You will need to download the version appropriate for your OS, unzip it, and place `elm-format` or `elm-format.exe` (windows) on your `PATH`.

 - Mac: [download](https://github.com/avh4/elm-format/releases/download/0.1-alpha2/elm-format-0.1-alpha2-mac-x64.tgz)
 - Linux: [download](https://github.com/avh4/elm-format/releases/download/0.1-alpha2/elm-format-0.1-alpha2-linux-x64.tgz)
 - Windows: [download](https://github.com/avh4/elm-format/releases/download/0.1-alpha2/elm-format-0.1-alpha2-win-x64.zip)

If the binary does not work for you, please [report details about your OS](https://github.com/avh4/elm-format/issues/new), or try [building from source](#building-from-source).

If you need PGP signatures, see the [release page](https://github.com/avh4/elm-format/releases/tag/0.1-alpha2).

### Editor integration

If you can simplify or improve the installation instructions or add instructions for another editor, please [make a pull request](https://github.com/avh4/elm-format/edit/master/README.md).

#### Integration with [Atom](https://atom.io/)

```bash
git clone https://github.com/avh4/atom-elm-format.git
cd atom-elm-format
apm install
```

If you can help improve the the [atom-elm-format package](https://github.com/avh4/atom-elm-format), or make a better package please do so!

#### Integration with Vim

Add the following to your vim config:

```vim
autocmd BufWritePost *.elm silent execute "!elm-format --yes %" | edit! | set filetype=elm
```

#### Integration with Sublime Text

1. Tools -> Build System -> New Build System...
2. Paste this in:

        {
            "cmd": ["/usr/local/bin/elm-format", "$file", "--yes"]
        }

3. Save that file as elm-format.sublime-build
4. Open a .elm file and go to Tools -> Build System -> elm-format
5. Install the SublimeOnSaveBuild plugin
6. Go to Preferences -> Package Preferences -> SublimeOnSaveBuild -> User
7. Paste this in:

        {
            "filename_filter": "\\.(elm)$",
            "build_on_save": 1
        }

8. Add this to your User preferences:

	        "show_panel_on_build": false

9. Make sure the elm-format binary is on your PATH
10. Profit!

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

```bash
cabal configure --enable-tests
./tests/run-tests.sh
```
