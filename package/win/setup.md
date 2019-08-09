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
# cp elm-format/elm-format-0.0.0-win-i386.zip ../..
