# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e # emacs keybinding?
# End of lines configured by zsh-newuser-install

# The following lines were added by compinstall
zstyle :compinstall filename '/home/user/.zshrc'

# End of lines added by compinstall

# my config
# prompt
autoload -Uz promptinit && promptinit   # load promptinit
PROMPT='%#%(?. ~> .%F{red} ~> %f)'      # left prompt, show # if priviliged else % and ~>, red if last command failed
RPROMPT='%~'                            # right prompt, show current dir

# help
autoload -Uz run-help run-help-git      # load run-help and run-help-git

# completition
autoload -Uz compinit && compinit       # load compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'   # match case insensitivly

setopt complete_aliases
setopt correctall

setopt autocd           # cd if not a command and dir exist
setopt extendedglob     # use extended globs, behaviour may be unexpected 

# variables
export EDITOR=nvim      # set editor to nvim
export VISUAL=nvim      # set visual editor to nvim

# aliases
alias reload='source $HOME/.zshrc'      # source .zshrc with reload command

alias vi=vim            # use vim instead of vi
alias vim=nvim          # use nvim instead of vim

alias sudo='sudo '                  # expand aliases when using with sudo
alias grep='grep --color=auto'      # color grep output
alias ls='ls --color=auto'          # color ls output
alias cal='cal -m --color=auto'     # color cal output and default to starting weekends with monday
