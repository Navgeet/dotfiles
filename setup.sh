#!/bin/bash

usage() { 
    echo -e "\nUsage: setup.sh [dotfiles] \n" 
}

if [  $# -le 0 ] 
then 
    usage
    exit 1
fi

sudo echo "nav   ALL=(ALL:ALL) NOPASSWD: ALL" >> /etc/sudoers

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 4773BD5E130D1D45 # spotify
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list


sudo apt update
sudo apt install -y termite zoom fonts-inconsolata awesome awsome-doc emacs nvidia-driver \
    suckless-tools htop firefox-est firefox-esr-l10n-hi-in fish spotify-client xbindkeys


# set fish as default shell
echo /usr/bin/fish | sudo tee -a /etc/shells
chsh -s /usr/bin/fish


DOTFILES=$(cd $1 | pwd)
cd $DOTFILES/

ln -s -t ~ .emacs.d .xinitrc .gitconfig .glocal_ignore .doom.d
~/.emacs.d/bin/doom sync

ln -s -t ~/.config awesome termite fish
