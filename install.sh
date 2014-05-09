#!/bin/bash
ln -s ~/.dotfiles/.emacs ~/.emacs
ln -s ~/.dotfiles/.bash_profile ~/.bash_profile
ln -s ~/.dotfiles/.bashrc ~/.bashrc

echo "[user]">> ~/.dotfiles/.git/config
echo "    name=cdtilde" >> ~/.dotfiles/.git/config
echo "    email=cdtilde@gmail.com" >> ~/.dotfiles/.git/config
