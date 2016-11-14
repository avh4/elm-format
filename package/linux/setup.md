
## First time setup

```bash
git clone ../.. elm-format
vagrant up
vagrant ssh
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442
  echo 'deb http://download.fpcomplete.com/ubuntu xenial main' | sudo tee /etc/apt/sources.list.d/fpco.list
  sudo apt-get update
  sudo apt-get install stack
  stack setup
```


## Build Linux x64 binary

```bash
vagrant ssh
  cd /vagrant/elm-format
  /vagrant/build-package.sh
vagrant halt
```
