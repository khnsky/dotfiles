autoload -Uz compinit && compinit       # load compinit
zstyle ':completion:*' rehash true      # persistent rehash
zstyle ':completion:*' menu select
# match case insensitively
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
