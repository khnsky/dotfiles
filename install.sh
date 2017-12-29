#!/bin/sh

dotfiles=$HOME/dotfiles

# .config
setup_config(){
    mkdir -p $HOME/.config
    for file in $dotfiles/.config; do
        ln -s $dotfiles/.config/$file $HOME/.config/$file
    done
}

# vim
setup_vim(){
    # make .vim directory, link .vimrc file
    mkdir -p $HOME/.vim
    ln -s $dotfiles/.vimrc $HOME/.vimrc
    # TODO: setup vim plugins
}

# nvim
setup_nvim(){
    ln -s $HOME/.vim $HOME/.config/nvim
    ln -s $HOME/.vimrc $HOME/.config/nvim/init.vim
}
