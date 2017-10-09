# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e #emacs keybinding?
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/user/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# 
# my config
export EDITOR=nvim
export VISUAL=nvim

alias reload='source $HOME/.zshrc'

alias vi=vim
alias vim=nvim

# use aliases with sudo
alias sudo='sudo '

#alias shutdown=/bin/shutdown

alias grep='grep --color=auto'
alias ls='ls --color=auto'
alias cal='cal -m --color=auto'
