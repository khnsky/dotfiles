# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt APPEND_HISTORY                   # append history instead of overwriting it
setopt INC_APPEND_HISTORY               # write history on command not on shell exit
setopt NO_SHARE_HISTORY                 # every instance of zsh has its history
setopt EXTENDED_HISTORY                 # store command start and execution time
setopt HIST_IGNORE_DUPS                 # dont save command if preciding command is same
setopt HIST_REDUCE_BLANKS               # strim meaningless whitespace
setopt HIST_IGNORE_SPACE                # dont save commands preceded with space

# The following lines were added by compinstall
zstyle :compinstall filename '/home/user/.zshrc'

# End of lines added by compinstall

# my config
bindkey -v                              # vi keybinding
export KEYTIMEOUT=1                     # timeout when exiting normal mode, default 40

# prompt {{{
autoload -Uz promptinit && promptinit   # load promptinit

setopt PROMPT_SUBST
setopt PROMPT_PERCENT

usr_p=
# $SSH_TTY, $SSH_CONNECTION, $SSH_CLIENT
if [ -n "$SSH_TTY" ] || [ -n "$SSH_CONNECTION" ] || [ -n "$SSH_CLIENT" ]; then
    # zsh builtin - whoami@hostname
    usr_p="[%n@%M]"
fi

autoload -Uz colors && colors
# can now use better color notation eg. fg[green]

good_color="%F{green}"
bad_color="%F{red}"
#color="%F{white}"
reset_color="%f"

color="%(?..$bad_color)"

privlige_p="[$color%#$reset_color]"     # '#' if shell running with priviliges, % otherwise
dir_p="[%~]"                            # show current dir, home shown as ~
git_p=
vim_p=

# doesn't work
#function zle-line-init zle-keymap-select {
#    vim_p="${${KEYMAP/vicmd/[ NORMAL ]}/(main|viins)/}"
#    zle reset-prompt
#}
#zle -N zle-line-init
#zle -N zle-keymap-select

#PROMPT='%(?.%B$: .%F{red}%B$: %f)%b'    # left prompt, $:, red if last command failed

PROMPT="${usr_p}${privlige_p}: "
RPROMPT="$vim_p$dir_p$git_p"

# }}}

# help
autoload -Uz run-help run-help-git  # load run-help and run-help-git

# completition
autoload -Uz compinit && compinit   # load compinit
zstyle ':completion:*' menu select
# match case insensitivly
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

setopt COMPLETE_ALIASES             # alias completition before substitution
setopt CORRECT_ALL

setopt AUTO_CD                      # cd if not a command and dir exist
setopt EXTENDED_GLOB                # use extended globs, behaviour may be unexpected

# variables
export EDITOR=nvim                  # set editor to nvim
export VISUAL=nvim                  # set visual editor to nvim

# functions
# stolen from /u/Rhomboid
cxxrun()
{
    local src=$1
    local out=/tmp/cxxrun-$RANDOM-$RANDOM-$RANDOM
    local flags="-xc++ -Wall -Wextra -pedantic -O2"
    local inc="-include iostream -include fstream -include iomanip \
        -include cmath -include vector -include cstdlib -include unistd.h \
        -include cstring -include cerrno -include fcntl.h"
    shift
    (
        trap 'rm -f "$out" 2>/dev/null 2>&1' EXIT
        eval ${CXX:-g++} -o "$out" $flags $inc - <<<"using namespace std; \
            int main(int argc, char* argv[]) { $src; }" "$@" && "$out"
    )
}

# aliases
alias reload='source $HOME/.zshrc'  # source .zshrc with reload command

alias vi=vim                        # use vim instead of vi
alias vim=nvim                      # use nvim instead of vim

alias sudo='sudo '                  # expand aliases when using with sudo

alias cal='cal -m --color=auto'     # color cal output, start week with monday
alias diff='diff --color=auto'      # color diff output
alias grep='grep --color=auto'      # color grep output
alias ls='ls --color=auto'          # color ls output
