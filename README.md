[![Build Status](https://travis-ci.org/avh4/elm-format.svg?branch=master)](https://travis-ci.org/avh4/elm-format)
![experimental](https://img.shields.io/badge/stability-experimental-orange.svg)
[![latest version: 0.1-alpha2](https://img.shields.io/badge/version-0.1--alpha2-blue.svg)](https://github.com/avh4/elm-format/releases/tag/0.1-alpha2)

# elm-format

> `elm-format` is still in alpha.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues).

`elm-format` formats [Elm](http://elm-lang.org) source code according to a standard set of rules. It is inspired by the popular [gofmt](https://blog.golang.org/go-fmt-your-code).

The benefits of `elm-format`:
 - It makes code **easier to write**, because you never have to worry about minor formatting concerns while powering out new code.
 - It makes code **easier to read**, because there are no longer distracting minor stylistic differences between different code bases. As such, your brain can map more efficiently from source to mental model.
 - It makes code **easier to maintain**, because you can no longer have diffs related only to formatting; every diff necessary involves a material change.
 - It **saves your team time** debating how to format things, because there is a standard tool that formats everything the same way.


## Usage

#### Format a file, saving to the same file after prompting to check if it's okay

```bash
elm-format Main.elm
```

#### Format a file, saving to the same file

```bash
elm-format Main.elm --yes
```

#### Format all files in a directory

```bash
elm-format src/
```

#### Format input from stdin and write to stdout

```bash
elm-format --stdin
```

#### Format input from stdin and write to file

```bash
elm-format --stdin --output Main.elm
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

> [atom-beautify](https://atom.io/packages/atom-beautify) 0.28.20 (not yet released) will support `elm-format`.

In the meantime:

```bash
git clone https://github.com/avh4/atom-elm-format.git
cd atom-elm-format
apm install
```

If you can help improve the the [atom-elm-format package](https://github.com/avh4/atom-elm-format), or make a better package please do so!

#### Integration with [Light Table](http://lighttable.com/)

Use the [elm-light](https://github.com/rundis/elm-light) plugin.

> If you can add more specific instructions for installing and configuring the elm-light plugin to work with `elm-format`, please [make a pull request](https://github.com/avh4/elm-format/edit/master/README.md).

#### Integration with Vim

Add the following to your vim config:

```vim
autocmd BufWritePost *.elm silent execute "!elm-format --yes %" | edit! | set filetype=elm
```

#### Integration with Sublime Text

1. Go to Tools -> Build System -> New Build System... and enter the following:

        { "cmd": ["/replace/this/with/your/path/to/elm-format", "$file", "--yes"] }
       
1. Save it as `elm-format.sublime-build`
1. Install the [SublimeOnSaveBuild](https://packagecontrol.io/packages/SublimeOnSaveBuild) package
1. Go to Preferences -> Package Settings -> SublimeOnSaveBuild -> User and enter this:

        { "filename_filter": "\\.elm$", "build_on_save": 1 }
        
1. To prevent it from bringing up the panel every time you build, open Preferences -> Settings -> User and add this setting:
`"show_panel_on_build": false`

## Development info

### Building from source

```bash
git clone https://github.com/avh4/elm-format.git
cd elm-format
cabal sandbox init --sandbox=.cabal-sandbox
cabal build
./dist/build/elm-format/elm-format --help
```

### Running tests

```bash
cabal configure --enable-tests
./tests/run-tests.sh
```
