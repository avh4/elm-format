Arch Linux AUR packages
=======================

AUR packages for Arch users are maintained by @mattjbray.

https://aur.archlinux.org/packages/elm-format-0.16-bin/

https://aur.archlinux.org/packages/elm-format-0.17-bin/

https://aur.archlinux.org/packages/elm-format-0.18-bin/

Updating the packages
---------------------

1. Create an AUR account at https://aur.archlinux.org.
2. Go to "My Account" and add your SSH public key.

```bash
cd elm-format/package/linux/archlinux

# Clone the AUR package repo:
git clone ssh+git://aur@aur.archlinux.org/elm-format-0.18-bin.git

# Bump `pkgver` in the `PKGBUILD` file.

# Launch the Arch virtual machine (if not already on an Arch host):
vagrant up
vagrant ssh
cd /vagrant/elm-format-0.18-bin

# Generate checksums:
makepkg -g

# Replace the checksums in `PKGBUILD` with the output of the previous command.

# Test the package:
makepkg -si
elm-format

# Update the `.SRCINFO` file:
makepkg --printsrcinfo > .SRCINFO

# Leave the Vagrant Arch VM:
exit

# Commit the changes and push:
cd elm-format-0.18-bin
git commit -am "Update to <version>"
git push origin master
```

Repeat the steps for the `elm-format-0.16-bin` and `elm-format-0.17-bin` packages.
