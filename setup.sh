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
sudo apt install -y alacritty awesome emacs rofi htop fish spotify-client blueman network-manager-applet keepassxc

# set fish as default shell
sudo chsh -s /usr/bin/fish nav

# add user to video group
sudo usermod -a -G video $LOGNAME

# DOTFILES=$(cd $1 | pwd)
# cd $DOTFILES/

# ln -rst ~ .xinitrc .gitconfig .glocal_ignore
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
ln -rst ~/.config awesome alacritty fish doom
~/.config/emacs/bin/doom sync
