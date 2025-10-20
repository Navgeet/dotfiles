#!/bin/bash

usage() { 
    echo -e "\nUsage: setup.sh [dotfiles] \n" 
}

if [  $# -le 0 ] 
then 
    usage
    exit 1
fi

sudo echo "nav   ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/nav

curl -sS https://download.spotify.com/debian/pubkey_C85668DF69375001.gpg | sudo gpg --dearmor --yes -o /etc/apt/trusted.gpg.d/spotify.gpg
echo "deb https://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list

sudo apt update
sudo apt install -y alacritty awesome awsome-doc emacs rofi htop fish spotify-client blueman network-manager-applet keepassxc

# set fish as default shell
echo /usr/bin/fish | sudo tee -a /etc/shells
sudo chsh -s /usr/bin/fish nav


DOTFILES=$(cd $1 | pwd)
cd $DOTFILES/

ln -s -t ~ .emacs.d .xinitrc .gitconfig .glocal_ignore .doom.d
~/.emacs.d/bin/doom sync

ln -s -t ~/.config awesome termite fish
