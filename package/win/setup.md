## First time setup

```bash
brew install caskroom/cask/brew-cask
brew cask install vagrant
brew cask install virtualbox
#vagrant init opentable/win-2012r2-standard-amd64-nocm
vagrant up
vagrant plugin install vagrant-winrm
vagrant winrm -c "dir"
```

## Start Windows VM

  - `(cd package/win && vagrant up)`
  - Open VirtualBox and show the runnning Windows VM
  - Start Git Bash (Right click on Windows Desktop -> "Git Bash Here")


## Provisioning (first time only)

  - Download and install Haskell Stack (32-bit)
  - `stack setup`
  - `choco install devbox-common`
  - `choco install poshgit`
  <!-- - #choco install sourcetree -->
  <!-- - #choco install tortoisegit -->
  - Restart shell


## Build Windows binary

From the host computer:

  - `cd package/win/elm-format`
  - `git fetch`

From the Windows VM:

  - `cd /c/vagrant/elm-format`
  - `git checkout <version tag>`
  - ## !! Make sure you are on the Windows VM !!
    `git clean -dfx`
    ## !! Make sure you are on the Windows VM !!
  - `/c/vagrant/build-package.sh`


## back to host computer
# cd ./package/win/
# vagrant halt

# on mac:  zip elm-format-0.1-alpha-2-win-x64.zip elm-format.exe
