#!/bin/bash
ln -s ~/.dotfiles/.emacs ~/.emacs

echo "[user]">> ~/.dotfiles/.git/config
echo "    name=cdtilde" >> ~/.dotfiles/.git/config
echo "    email=cdtilde@gmail.com" >> ~/.dotfiles/.git/config
