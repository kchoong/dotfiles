#!/bin/bash

# A script to copy all my dotfiles

# config files
rm -r .config
cp -r ~/.config .
cp ~/.bashrc .
cp ~/.xinitrc .
cp ~/.Xresources .

# remove unnecessary config
rm -r .config/Atom
rm -r .config/chromium
rm -r .config/configstore
rm -r .config/dconf
rm -r .config/discord
rm -r .config/gtk-3.0
rm -r .config/menus
rm -r .config/procps
rm -r .config/rclone
rm -r .config/spotify
rm -r .config/vlc
rm -r .config/yay

# apache, mysql & php
rm -r etc
mkdir etc
cp -r /etc/php .
cp -r /etc/httpd .
rm httpd/modules
mv php etc
mv httpd etc

# pacman
pacman -Qqe > pkglist.txt
pacman -Qqem > aurpkglist.txt

# scripts
rm -r .scripts
cp -r ~/.scripts .
