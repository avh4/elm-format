[![Build Status](https://travis-ci.org/avh4/elm-format.svg?branch=master)](https://travis-ci.org/avh4/elm-format)
[![latest version: 0.8.2](https://img.shields.io/badge/version-0.8.2-orange.svg)](https://github.com/avh4/elm-format/releases/tag/0.8.2)

# elm-format

> `elm-format` is still in beta.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues).
>
> **The format produced by elm-format may change significantly before the 1.0.0 release.**  If this will cause problems for you, please refrain from using elm-format during the beta-test period.

`elm-format` formats [Elm](https://elm-lang.org) source code according to a standard set of rules based on [the official Elm Style Guide](https://elm-lang.org/docs/style-guide). It is inspired by the popular [gofmt](https://blog.golang.org/go-fmt-your-code).

The benefits of `elm-format`:
 - It makes code **easier to write**, because you never have to worry about minor formatting concerns while powering out new code.
 - It makes code **easier to read**, because there are no longer distracting minor stylistic differences between different code bases. As such, your brain can map more efficiently from source to mental model.
 - It makes code **easier to maintain**, because you can no longer have diffs related only to formatting; every diff necessarily involves a material change.
 - It **saves your team time** debating how to format things, because there is a standard tool that formats everything the same way.
 - It **saves you time** because you don't have to nitpick over formatting details of your code.


## Usage

```bash
elm-format Main.elm  # Format a single file
elm-format Main.elm --yes  # Overwrite the file without prompting
elm-format src/  # Format all *.elm files in a directory
elm-format --stdin  # Format input from stdin and write to stdout
elm-format --stdin --output Main.elm  # Format input from stdin and write to file
elm-format --help  # See other command line options
```


## Installation [![(latest version: 0.8.2)](https://img.shields.io/badge/version-0.8.2-orange.svg)](https://github.com/avh4/elm-format/releases/tag/0.8.2)

> `elm-format` is still in beta.  If you run into any problems, please [report them](https://github.com/avh4/elm-format/issues).
>
> **The format produced by elm-format may change significantly before the 1.0.0 release.**  If this will cause problems for you, please refrain from using elm-format during the beta-test period.

To install `elm-format`:

```sh
npm install -g elm-format
```

or download the version appropriate for your OS from the [release page](https://github.com/avh4/elm-format/releases/tag/0.8.2),
unzip it,
and place `elm-format` or `elm-format.exe` (windows) on your `PATH`.

On FreeBSD you can `pkg install hs-elm-format` or use [devel/elm-format](https://www.freshports.org/devel/elm-format/), then execute `elm-format` normally.

You must run `elm-format` from the directory that contains your `elm.json` (for Elm 0.19) or `elm-package.json` (for Elm 0.18),
or else you must pass the appropriate `--elm-version=0.19`/`--elm-version=0.18` command line argument.


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
    <td rowspan=1>Visual Studio Code</td>
    <td>:trophy: <a href="https://marketplace.visualstudio.com/items?itemName=sbrink.elm">Elm Language Support</a></td>
    <td>:warning: <a href="#visual-studio-code-installation">3 steps</a></td>
    <td>:white_check_mark:</td>
    <td>:warning: requires configuration</td>
    <td>❔ TBD</td>
    <td>❔ TBD</td>
  </tr>
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
    <td>:trophy: <a href="https://klazuka.github.io/intellij-elm/">intellij-elm</a></td>
    <td>:warning: <a href="#jetbrains-installation">4 steps</a></td>
    <td>❔ TBD</td>
    <td>:white_check_mark:</td>
    <td>❔ TBD</td>
    <td>❔ TBD</td>
  </tr>
  <tr>
    <td rowspan=1>TextMate</td>
    <td>:trophy: <a href="https://github.com/cmason/Elm.tmBundle">Elm.tmbundle</a></td>
    <td>:white_check_mark: <a href="https://github.com/cmason/Elm.tmBundle#installation">2 steps</a></td>
    <td>❔ TBD</td>
    <td>❔ TBD</td>
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
1. To format on save, edit your user keymap by performing the following:
  * Click File -> Settings -> User Keymap to open the user keymap.
  * Copy the following line and paste it into your keymap. Anywhere is fine as long as it is whithin the outer brackets. Ensure to save the file.
  ```clojure
  [:editor.elm "ctrl-s" :save :elm-format :elm.lint]
  ```
  * Search for "App: Reload keymaps" in the Commands Window to apply the changes (or restart LightTable).


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

> Note: If you previously installed a VSCode extension called "elm-format", uninstall it (it is deprecated, and the "elm" extension now provides elm-format integration).

1. Install elm-format
1. Install the extension [Elm Language Support](https://marketplace.visualstudio.com/items?itemName=sbrink.elm) for VSCode, which includes syntax and error highlighting
1. Configure the extension to format on save:

    1. Find your `settings.json` file ([instructions](https://code.visualstudio.com/docs/getstarted/settings#_settings-file-locations)).
    1. Add the following key-value pair to your `settings.json`:

    ```json
    "[elm]": {
        "editor.formatOnSave": true
    },
    ```


### Sublime Text installation

1. Install elm-format
1. Install the [Elm Language Support](https://packagecontrol.io/packages/Elm%20Language%20Support) package.


### JetBrains installation

This is for WebStorm and other JetBrains IDEs like IntelliJ and PyCharm.

1. Install elm-format
1. Install the [intellij-elm plugin](https://klazuka.github.io/intellij-elm/)
1. In IntelliJ, open Settings -> Languages & Frameworks -> Elm
1. Specify the path to elm-format



## Development info

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### Building from source

```bash
# check out the repo
brew install haskell-stack
git clone https://github.com/avh4/elm-format.git
cd elm-format

# initial setup
stack setup
stack install shake

# build
stack runhaskell Shakefile.hs -- build

# run the built elm-format
"$(stack path --local-install-root)"/bin/elm-format
```

### Running tests

```bash
stack install shake
stack runhaskell Shakefile.hs
```
