[![Build Status](https://travis-ci.org/avh4/elm-format.svg?branch=master)](https://travis-ci.org/avh4/elm-format)
![experimental](https://img.shields.io/badge/stability-experimental-orange.svg)
[![latest version: 0.3.1-alpha](https://img.shields.io/badge/version-0.3.1--alpha-blue.svg)](https://github.com/avh4/elm-format/releases/tag/0.3.1-alpha)

# elm-format

> `elm-format` is still in alpha.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues).
>
> **The format produced by elm-format may change significantly before the 1.0.0 release.**  If this will cause problems for you, please refrain from using elm-format during the alpha- and beta-test periods.

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

## Installation [![(latest version: 0.3.1-alpha)](https://img.shields.io/badge/version-0.3.1--alpha-blue.svg)](https://github.com/avh4/elm-format/releases/tag/0.3.1-alpha)

> `elm-format` is still in alpha.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues).
>
> **The format produced by elm-format may change significantly before the 1.0.0 release.**  If this will cause problems for you, please refrain from using elm-format during the alpha- and beta-test periods.

You will need to download the version appropriate for your OS, unzip it, and place `elm-format` or `elm-format.exe` (windows) on your `PATH`.  Simpler installation options will be available once there is a stable release of elm-format.

If you need PGP signatures, see the [releases page](https://github.com/avh4/elm-format/releases).

### For Elm 0.17

(Using this version with Elm 0.16 files will migrate them to Elm 0.17 syntax.)

 - Mac: [download](https://github.com/avh4/elm-format/releases/download/0.3.1-alpha/elm-format-0.17-0.3.1-alpha-mac-x64.tgz)
 - Linux: [download](https://github.com/avh4/elm-format/releases/download/0.3.1-alpha/elm-format-0.17-0.3.1-alpha-linux-x64.tgz)
 - Windows: [download](https://github.com/avh4/elm-format/releases/download/0.3.1-alpha/elm-format-0.17-0.3.1-alpha-win-x64.zip)

### For Elm 0.16

 - Mac: [download](https://github.com/avh4/elm-format/releases/download/0.3.1-alpha/elm-format-0.16-0.3.1-alpha-mac-x64.tgz)
 - Linux: [download](https://github.com/avh4/elm-format/releases/download/0.3.1-alpha/elm-format-0.16-0.3.1-alpha-linux-x64.tgz)
 - Windows: [download](https://github.com/avh4/elm-format/releases/download/0.3.1-alpha/elm-format-0.16-0.3.1-alpha-win-x64.zip)

### Editor integration

If you can simplify or improve the installation instructions or add instructions for another editor, please [make a pull request](https://github.com/avh4/elm-format/edit/master/README.md).  The default behavior of `elm-format`-approved plugins is to format Elm files on save.

<!-- Open-source editors will be listed before closed-source editors. -->


#### Integration with [Atom](https://atom.io/)

[atom-elm-format](https://atom.io/packages/elm-format) supports `elm-format`.  You can install `elm-format` using `apm` or the Atom package manager in Atom's settings.

[atom-beautify](https://atom.io/packages/atom-beautify) 0.28.20 and above supports `elm-format`.  You can install `atom-beautify` using `apm` or the Atom package manager in Atom's settings.


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

1. Install [elm-mode](https://github.com/jcollard/elm-mode) ([instructions](https://github.com/jcollard/elm-mode)).
1. Make sure `elm-format` is in your PATH.

That's all.

After the install C-c C-f (in `elm-mode`) runs the command elm-mode-format-buffer which is based on `elm-format`.

See also the respective section about `elm-format` at [elm-mode](https://github.com/jcollard/elm-mode#elm-format)


#### Integration with Sublime Text

Use the [Elm Language Support](https://packagecontrol.io/packages/Elm%20Language%20Support) package. Make sure elm-format is in your PATH.


## Development info

### Building from source

```bash
git clone https://github.com/avh4/elm-format.git
cd elm-format
cabal sandbox init
cabal install --only-dependencies --enable-tests
cabal build
./dist/build/elm-format-0.17/elm-format-0.17 --help
```

### Running tests

```bash
brew install shellcheck
cabal configure --enable-tests
./tests/run-tests.sh
```
