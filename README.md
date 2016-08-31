[![Build Status](https://travis-ci.org/avh4/elm-format.svg?branch=master)](https://travis-ci.org/avh4/elm-format)
![experimental](https://img.shields.io/badge/stability-experimental-orange.svg)
[![latest version: 0.4.0-alpha](https://img.shields.io/badge/version-0.4.0--alpha-blue.svg)](https://github.com/avh4/elm-format/releases/tag/0.4.0-alpha)

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

## Installation [![(latest version: 0.4.0-alpha)](https://img.shields.io/badge/version-0.4.0--alpha-blue.svg)](https://github.com/avh4/elm-format/releases/tag/0.4.0-alpha)

> `elm-format` is still in alpha.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues).
>
> **The format produced by elm-format may change significantly before the 1.0.0 release.**  If this will cause problems for you, please refrain from using elm-format during the alpha- and beta-test periods.

You will need to download the version appropriate for your OS, unzip it, and place `elm-format` or `elm-format.exe` (windows) on your `PATH`.  Simpler installation options will be available once there is a stable release of elm-format.

If you need PGP signatures, see the [releases page](https://github.com/avh4/elm-format/releases).

### For Elm 0.17

(Using this version with Elm 0.16 files will migrate them to Elm 0.17 syntax.)

 - Mac: [download](https://github.com/avh4/elm-format/releases/download/0.4.0-alpha/elm-format-0.17-0.4.0-alpha-mac-x64.tgz)
 - Linux: [download](https://github.com/avh4/elm-format/releases/download/0.4.0-alpha/elm-format-0.17-0.4.0-alpha-linux-x64.tgz)
 - Windows: [download](https://github.com/avh4/elm-format/releases/download/0.4.0-alpha/elm-format-0.17-0.4.0-alpha-win-x64.zip)

### For Elm 0.16

 - Mac: [download](https://github.com/avh4/elm-format/releases/download/0.4.0-alpha/elm-format-0.16-0.4.0-alpha-mac-x64.tgz)
 - Linux: [download](https://github.com/avh4/elm-format/releases/download/0.4.0-alpha/elm-format-0.16-0.4.0-alpha-linux-x64.tgz)
 - Windows: [download](https://github.com/avh4/elm-format/releases/download/0.4.0-alpha/elm-format-0.16-0.4.0-alpha-win-x64.zip)


## Editor integration

<!-- Open-source editors will be listed before closed-source editors. -->

Find your editor in the table below.  The recommended plugin for each editor is indicated with :trophy: (trophy emoji).

<table>
  <tr>
    <th>Editor</th>
    <th>Plugin</th>
    <th>Installation</th>
    <th><a href="https://github.com/avh4/elm-format/issues/104">Formatting</a></th>
    <th><a href="https://github.com/avh4/elm-format/issues/104">Format on save</a></th>
    <th><a href="https://github.com/avh4/elm-format/issues/104">Configuration</a></th>
    <th><a href="https://github.com/avh4/elm-format/issues/104">Error handling</a></th>
  </tr>
  <tr>
    <td rowspan=2><a href="https://atom.io/">Atom</a></td>
    <td>:trophy: <a href="https://atom.io/packages/elm-format">atom-elm-format</a></td>
    <td>:white_check_mark: <a href="#atom-elm-format-installation">2 steps</a></td>
    <td>:white_check_mark:</td>
    <td>:white_check_mark:</td>
    <td>:white_check_mark:</td>
    <td>:warning: no installation instructions</td>
  </tr>
  <tr>
    <!-- Atom -->
    <td><a href="https://atom.io/packages/atom-beautify">atom-beautify</a></td>
    <td>:warning: <a href="#atom-beautify-installation">3 steps</a></td>
    <td>:white_check_mark:</td>
    <td>:warning: requires configuration</td>
    <td>:white_check_mark:</td>
    <td>:white_check_mark:</td>
  </tr>
  <tr>
    <td rowspan=1><a href="http://lighttable.com/">Light Table</a></td>
    <td>:trophy: <a href="https://github.com/rundis/elm-light">elm-light</a></td>
    <td>:warning: <a href="#elm-light-installation">3 steps</a></td>
    <td>:white_check_mark:</td>
    <td>:warning: requires configuration</td>
    <td>:white_check_mark:</td>
    <td>:warning: no installation instructions</td>
  </tr>
  <tr>
    <td rowspan=1>Vim</td>
    <td>:trophy: <a href="https://github.com/ElmCast/elm-vim">elm-vim</a></td>
    <td>:x: <a href="#elm-vim-installation">6 steps</a></td>
    <td>:white_check_mark:</td>
    <td>⚠️ requires configuration</td>
    <td>:white_check_mark:</td>
    <td>:x: no error message</td>
  </tr>
  <tr>
    <td rowspan=1>Emacs</td>
    <td>:trophy: <a href="https://github.com/jcollard/elm-mode">elm-mode</a></td>
    <td>:x: <a href="#elm-mode-installation">4 steps</a></td>
    <td>:white_check_mark:</td>
    <td>:warning: requires configuration</td>
    <td>:white_check_mark:</td>
    <td>:warning: no installation instructions</td>
  </tr>
  <tr>
    <td rowspan=2>Visual Studio Code</td>
    <td>:trophy: <a href="https://marketplace.visualstudio.com/items?itemName=sbrink.elm">Elm Language Support</a></td>
    <td>:warning: <a href="#visual-studio-code-installation">3 steps</a></td>
    <td>❔ TBD</td>
    <td>:x:</td>
    <td>❔ TBD</td>
    <td>❔ TBD</td>
  </tr>
  <tr>
    <!-- Visual Studio Code -->
    <td><a href="https://marketplace.visualstudio.com/items?itemName=abadi199.elm-format">VSCode Elm Format</a></td>
    <td>:warning: <a href="#vscode-elm-format-installation">3 steps</a></td>
    <td>❔ TBD</td>
    <td>:warning: requires configuration</td>
    <td>❔ TBD</td>
    <td>❔ TBD</td>
  </tr>
  <tr>
    <td rowspan=1>Sublime Text</td>
    <td>:trophy: <a href="https://packagecontrol.io/packages/Elm%20Language%20Support">Elm Language Support</a></td>
    <td>:white_check_mark: <a href="#sublime-text-installation">2 steps</a></td>
    <td>❔ TBD</td>
    <td>:white_check_mark:</td>
    <td>❔ TBD</td>
    <td>❔ TBD</td>
  </tr>
  <tr>
    <td rowspan=1>JetBrains (WebStorm, etc)</td>
    <td>:trophy: <a href="https://durkiewicz.github.io/elm-plugin/">Elm Language Plugin</a></td>
    <td>:warning: <a href="#webstorm-installation">3 steps</a></td>
    <td>❔ TBD</td>
    <td>:white_check_mark:</td>
    <td>❔ TBD</td>
    <td>❔ TBD</td>
  </tr>
</table>


## Detailed instructions

If you can simplify or improve the installation instructions or add instructions for another editor, please [make a pull request](https://github.com/avh4/elm-format/edit/master/README.md).
The default behavior of `elm-format`-approved plugins is to format Elm files on save.


### atom-elm-format installation

1. Install elm-format
1. Install atom-elm-format

    ```
    apm install elm-format
    ```
    
  or use the Atom package manager in Atom's settings


### atom-beautify installation

1. Install elm-format
1. Install atom-beautify

    ```
    apm install atom-beautify
    ```
    
  or use the Atom package manager in Atom's settings

1. Use `^⌥B` (`CTRL-ALT-B`) to format a file


### elm-light installation

1. Install elm-format
1. Install the [elm-light plugin](https://github.com/rundis/elm-light) using the Light Table plugin manager
1. Add the following to your user keymap:

  ```clojure
  [:editor.elm "ctrl-s" :save :elm-format :elm.lint]
  ```
  
  > This step needs improvement to be understandable by novice Light Table users:
  > how does one edit the user keymap?


### elm-mode installation

1. Install elm-format
1. If your Emacs has `package.el` (which is automatically the case for Emacs >= 24), you can install `elm-mode` from the package in [MELPA](http://melpa.milkbox.net/):

    1. Ensure that you have added the MELPA source in your `~/.emacs.d/init.el`:

        ```lisp
        (require 'package)
        (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
        ```
        
    1. Install elm-mode ([official instructions](https://github.com/jcollard/elm-mode#installation)): Use `M-x list-packages` and choose `elm-mode`.

1. Set `elm-format-on-save` to `t` to apply elm-format on the current buffer on every save. (The setting can be changed via `M-x customize-variable elm-format-on-save`. Click button `Toggle` to change the setting and button `State` to activate the setting.) 


### elm-vim installation

1. Install elm-format
1. Install [vim-plug](https://github.com/junegunn/vim-plug) ([official instructions](https://github.com/junegunn/vim-plug#installation))
   
    1. Download vim-plug:
    
        ```bash
        curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        ```
 
   1. Make sure there is a section like this in your `~/.vimrc`:
   
        ```vim
        call plug#begin('~/.vim/plugged')
        " ... any active plugins
        call plug#end()
        ```
   
 

1. Install elm-vim ([official instructions](https://github.com/ElmCast/elm-vim#install))

    1. Add `Plug 'elmcast/elm-vim'` to the `plug#begin` plugin section in your `~/.vimrc`
    1. Start `vim` and run `:PlugInstall`

1. Add the following to your `~/.vimrc`:

  ```
  let g:elm_format_autosave = 1
  ```


### Visual Studio Code installation

1. Install elm-format
1. Install Elm tools for VSCode

    ```bash
    ext install elm
    ```

1. SHIFT-ALT-F will format the current file


### VSCode Elm Format installation

1. Install elm-format
1. Install VSCode Elm Format

    ```bash
    ext install elm-format
    ```

1. You can run elm-format by using the `Elm: Format` command
1. You can also run elm-format whenever you save the file by enabling the `formatOnSave' option

    > This step needs improvement to be understandable by novice Visual Studio Code users:
    > how does one enable the `formatOnSave` option?


### Sublime Text installation

1. Install elm-format
1. Install the [Elm Language Support](https://packagecontrol.io/packages/Elm%20Language%20Support) package.


### JetBrains installation

This is for WebStorm and other JetBrains IDEs.

1. Install elm-format
1. Install the [Elm Language Plugin](https://durkiewicz.github.io/elm-plugin/) package.
1. Add a file watcher for .elm files with the settings as [shown here](img/JetBrains%20setup.png).


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
