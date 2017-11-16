```bash
brew install caskroom/cask/brew-cask
brew cask install vagrant
brew cask install virtualbox
#vagrant init opentable/win-2012r2-standard-amd64-nocm
vagrant up
vagrant plugin install vagrant-winrm
vagrant winrm -c "dir"
```

# Open VirtualBox and show the Windows VM

# Download and install Haskell Stack (32-bit)

stack setup
choco install devbox-common
choco install poshgit
#choco install sourcetree
#choco install tortoisegit

# Restart shell


## Build Windows binary

```bash
cd package/win/elm-format
git fetch
git checkout <version tag>

## !! Make sure you are in package/win/elm-format !!
git clean -dfx
## !! Make sure you are in package/win/elm-format !!
```

 - Start Git Bash

```bash
cd /c/vagrant/elm-format
/c/vagrant/build-package.sh
```

## back to host computer
# cd ./package/win/
# vagrant halt

# on mac:  zip elm-format-0.1-alpha-2-win-x64.zip elm-format.exe
