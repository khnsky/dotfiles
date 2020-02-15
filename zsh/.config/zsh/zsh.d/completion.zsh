autoload -Uz compinit && compinit       # load compinit

zstyle ':completion:*' rehash true      # persistent rehash
zstyle ':completion:*' menu select

# smart case - lowercase matches both upper and lower case letters
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
