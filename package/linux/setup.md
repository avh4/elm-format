
```bash
git clone ../.. elm-format
vagrant up
vagrant ssh
  sudo apt-get install git
  sudo add-apt-repository -y ppa:hvr/ghc
  sudo apt-get update
  sudo apt-get install ghc-7.10.3
  sudo apt-get install cabal-install-1.22
```


## Build Linux x64 binary

```bash
vagrant ssh
  export PATH=/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH
  cd /vagrant/elm-format
  cabal update
  /vagrant/build-package.sh
vagrant halt
```
