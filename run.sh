#!/bin/bash

# A script to copy all my dotfiles

# remove old folders
rm -r .config
rm -r .scripts
rm -r .xmonad
rm -r etc

# recreating folders
mkdir .config
mkdir .xmonad
mkdir etc

# home config files
cp ~/.bashrc .
cp ~/.xinitrc .
cp ~/.Xresources .
cp -r ~/.scripts .

# config files

cp -r ~/.config/git .config/
cp -r ~/.config/hg .config/
cp -r ~/.config/picom .config/
cp -r ~/.config/rofi .config/

# xmonad
cp ~/.xmonad/xmonad.hs .xmonad/
cp ~/.xmobarrc .

# apache
mkdir etc/httpd
cp -r /etc/httpd/conf etc/httpd/

# php
cp -r /etc/php etc/

# mysql
cp /etc/my.cnf etc/
cp -r /etc/my.cnf.d etc/

# hostname
cp /etc/hostname etc/

# hosts
cp /etc/hosts etc/

# xorg
mkdir etc/X11
cp -r /etc/X11/xorg.conf.d etc/X11/

# pacman
pacman -Qqe > pkglist.txt
pacman -Qqem > aurpkglist.txt
