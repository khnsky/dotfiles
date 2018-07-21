# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

setopt APPEND_HISTORY                   # append history instead of overwriting it
setopt EXTENDED_HISTORY                 # store command start and execution time
setopt HIST_IGNORE_DUPS                 # don't save command if preceding command is same
setopt HIST_IGNORE_SPACE                # don't save commands preceded with space
setopt HIST_REDUCE_BLANKS               # trim meaningless whitespace
setopt INC_APPEND_HISTORY               # write history on command not on shell exit
setopt NO_SHARE_HISTORY                 # every instance of zsh has its history

# The following lines were added by compinstall
zstyle :compinstall filename '/home/user/.zshrc'

# End of lines added by compinstall

# my config
bindkey -v                              # vi keybinding
export KEYTIMEOUT=1                     # timeout when exiting normal mode, default 40

# prompt {{{
autoload -Uz promptinit && promptinit   # load promptinit
setopt PROMPT_PERCENT                   # perform % sequences substitution
setopt PROMPT_SUBST                     # perform substitution in prompt
setopt TRANSIENT_RPROMPT                # display rprompt only on current line

autoload -Uz vcs_info
autoload -Uz colors && colors
zstyle ':vcs_info:*' enable git
# b - branch, a - action (rebase), m - misc. (stashes), u - unstaged, c -staged
zstyle ':vcs_info:*' formats "%r :: %b %m%u%c"
zstyle ':vcs_info:*' actionformats "%r :: %b|%a %m%u%c"
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr "%F{green}+%f"   # show green + if staged files
zstyle ':vcs_info:*' unstagedstr "%F{red}+%f"   # show red + if unstaged files

precmd() {
    vcs_info                                    # set vcs_info...
    if [ -n "$vcs_info_msg_0_" ]; then
        RPROMPT="$vcs_info_msg_0_"
    else
        RPROMPT="%~"
    fi
}
PROMPT=' %(?..%F{red})%#%f :: > '

# }}}

# help
autoload -Uz run-help run-help-git  # load run-help and run-help-git

# completion
autoload -Uz compinit && compinit   # load compinit
zstyle ':completion:*' rehash true  # persistent rehash
zstyle ':completion:*' menu select
# match case insensitively
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

setopt COMPLETE_ALIASES             # alias completion before substitution
setopt CORRECT_ALL

setopt AUTO_CD                      # cd if not a command and dir exist
setopt EXTENDED_GLOB                # use extended globs, behavior may be unexpected

# variables
export VISUAL=emacsclient           # set visual editor to emacsclient
export ALTERNATE_EDITOR=''          # start emacs --daemon if not running

flags='-Wall -Wextra '
flags+='-Wduplicated-cond -Wduplicated-branches -Wlogical-op '
flags+='-Wrestrict -Wnull-dereference '
flags+='-Wdouble-promotion -Wconversion -Wsign-conversion '
flags+='-Wshadow -Wformat=2 -g '
export CFLAGS="-std=c11 $flags -Wjump-misses-init -pedantic"
export CXXFLAGS="$flags -Wold-style-cast -Wuseless-cast"
export LDFLAGS="-lm"
unset flags

# color man pages
# boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
# -R: output ANSI color escape seqs in 'raw' form,
# +Gg: go to the end and back to show total lines and percentage,
# represent termcap effect as:
# ..._md: begin bold - headings (bold; blue fg)
# ..._us: begin underline - proper names (bright yellow fg)
# ..._so: begin standout mode - prompt at the bottom (bold; bright white fg; black bg)
# ..._me: reset all mode (so, us, mb, md, mr)
# ..._se: reset standout mode
# ..._ue: reset underline
# ..._mb: begin blink - unused
man() {
    LESS="-R +Gg" \
    LESS_TERMCAP_md=$'\e[1;34m' \
    LESS_TERMCAP_us=$'\e[93m' \
    LESS_TERMCAP_so=$'\e[01;97;40m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_ue=$'\e[0m'  \
    LESS_TERMCAP_mb=$'\e[1;36m' \
    command man "$@"
}

# functions
# stolen from /u/Rhomboid
cxxrun() {
    local src=$1
    local out=/tmp/cxxrun-$RANDOM-$RANDOM-$RANDOM
    local flags="-xc++ -Wall -Wextra -g"
    local inc="-include iostream -include fstream -include iomanip \
        -include cmath -include vector -include cstdlib -include unistd.h \
        -include cstring -include cerrno -include fcntl.h"
    shift
    (
        trap 'rm -f "$out" 2>/dev/null 2>&1' EXIT
        eval ${CXX:-g++} -o "$out" $flags $inc - <<< "using namespace std; \
            int main(int argc, char* argv[]) { $src; }" "$@" && "$out"
    )
}

# presenting
mirror-screen() {
    if [ $# -lt 3 ]; then
        echo 'usage: mirror-screen _origin-display_ _dest-display_ _resolution_'
        xrandr | grep --color=never ' connected'
        return 1
    fi
    xrandr --output $2 --auto --scale-from $3 --output $1
}

# aliases
alias reload='. $HOME/.zshrc'           # source .zshrc with reload command

alias sudo='sudo '                      # expand aliases when using with sudo

# auto color output
alias cal='cal -m --color=auto'         # color cal output, start week with Monday
alias diff='diff --color=auto'          # color diff output
alias grep='grep --color=auto'          # color grep output
alias ls='ls --color=auto'              # color ls output

alias rustc='TERM=xterm-color rustc'    # shitty workaround for colored output


type cgdb > /dev/null 2>&1 && alias gdb='cgdb'
