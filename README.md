[![Build Status](https://travis-ci.org/avh4/elm-format.svg?branch=master)](https://travis-ci.org/avh4/elm-format)
![experimental](https://img.shields.io/badge/stability-experimental-orange.svg)
[![latest version: 0.2.0-alpha](https://img.shields.io/badge/version-0.2.0--alpha-blue.svg)](https://github.com/avh4/elm-format/releases/tag/0.2.0-alpha)

# elm-format

> `elm-format` is still in alpha.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues).

`elm-format` formats [Elm](http://elm-lang.org) source code according to a standard set of rules. It is inspired by the popular [gofmt](https://blog.golang.org/go-fmt-your-code).

The benefits of `elm-format`:
 - It makes code **easier to write**, because you never have to worry about minor formatting concerns while powering out new code.
 - It makes code **easier to read**, because there are no longer distracting minor stylistic differences between different code bases. As such, your brain can map more efficiently from source to mental model.
 - It makes code **easier to maintain**, because you can no longer have diffs related only to formatting; every diff necessarily involves a material change.
 - It **saves your team time** debating how to format things, because there is a standard tool that formats everything the same way.


## Usage

```bash
elm-format Main.elm  # Format a single file
elm-format Main.elm --yes  # Overwrite the file without prompting
elm-format src/  # Format all *.elm files in a directory
elm-format --stdin  # Format input from stdin and write to stdout
elm-format --stdin --output Main.elm  # Format input from stdin and write to file
elm-format --help  # See other command line options
```

## Installation [![(latest version: 0.2.0-alpha)](https://img.shields.io/badge/version-0.2.0--alpha-blue.svg)](https://github.com/avh4/elm-format/releases/tag/0.2.0-alpha)

`elm-format` is still in alpha.  You will need to download the version appropriate for your OS, unzip it, and place `elm-format` or `elm-format.exe` (windows) on your `PATH`.

 - Mac: [download](https://github.com/avh4/elm-format/releases/download/0.2.0-alpha/elm-format-0.2.0-alpha-mac-x64.tgz) or `brew tap homebrew/devel-only; brew install --devel elm-format`
 - Linux: [download](https://github.com/avh4/elm-format/releases/download/0.2.0-alpha/elm-format-0.2.0-alpha-linux-x64.tgz)
 - Windows: [download](https://github.com/avh4/elm-format/releases/download/0.2.0-alpha/elm-format-0.2.0-alpha-win-x64.zip)

If the binary does not work for you, please [report details about your OS](https://github.com/avh4/elm-format/issues/new), or try [building from source](#building-from-source).

If you need PGP signatures, see the [releases page](https://github.com/avh4/elm-format/releases).

### Editor integration

If you can simplify or improve the installation instructions or add instructions for another editor, please [make a pull request](https://github.com/avh4/elm-format/edit/master/README.md).  The default behavior of `elm-format`-approved plugins is to format Elm files on save.

<!-- Open-source editors will be listed before closed-source editors. -->


#### Integration with [Atom](https://atom.io/)

[atom-beautify](https://atom.io/packages/atom-beautify) 0.28.20 and above supports `elm-format`.  You can install `atom-beautify` using `apm` or the Atom package manager in Atom's settings.

[atom-elm-format](https://atom.io/packages/elm-format) supports `elm-format`.  You can install `elm-format` using `apm` or the Atom package manager in Atom's settings.


#### Integration with [Light Table](http://lighttable.com/)

1. Makes sure `elm-format` is in your PATH
1. Install the [elm-light plugin](https://github.com/rundis/elm-light) using the Light Table plugin manager
1. Add the following to your user keymap:

  ```clojure
  [:editor.elm "ctrl-s" :save :elm-format :elm.lint]
  ```


#### Integration with Vim

1. Make sure `elm-format` is in your PATH.
1. Install [elm-vim](https://github.com/ElmCast/elm-vim) ([instructions](https://github.com/ElmCast/elm-vim#install))
1. Add the following to your `.vimrc` file:

  ```
  let g:elm_format_autosave = 1
  ```


#### Integration with Emacs

Use [elm-mode](https://github.com/jcollard/elm-mode#elm-format).

> If you can add more specific instructions for installing and configuring elm-mode to work with `elm-format`, please [make a pull request](https://github.com/avh4/elm-format/edit/master/README.md).


#### Integration with Sublime Text

Use the [Elm Language Support](https://packagecontrol.io/packages/Elm%20Language%20Support) package. Make sure elm-format is in your PATH.


## Development info

### Building from source

```bash
git clone https://github.com/avh4/elm-format.git
cd elm-format
cabal sandbox init --sandbox=.cabal-sandbox
cabal install --only-dependencies --enable-tests
cabal build
./dist/build/elm-format/elm-format --help
```

### Running tests

```bash
brew install shellcheck
cabal configure --enable-tests
./tests/run-tests.sh
```
