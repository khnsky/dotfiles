# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

# The following lines were added by compinstall
zstyle :compinstall filename '/home/user/.zshrc'
# End of lines added by compinstall

HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

bindkey -v                              # vi keybinding
KEYTIMEOUT=1                            # timeout when exiting normal mode, default 40

DIRSTACKSIZE=10

setopt AUTO_CD                          # cd if not a command and dir exist
setopt AUTO_PUSHD                       # add dir to dirstack on cd
setopt COMPLETE_ALIASES                 # alias completion before substitution
setopt CORRECT_ALL
setopt EXTENDED_GLOB                    # use extended globs, behavior may be unexpected
setopt EXTENDED_HISTORY                 # store command start and execution time
setopt HIST_IGNORE_DUPS                 # don't save consecutive duplicate commands
setopt HIST_IGNORE_SPACE                # don't save commands preceded with space
setopt HIST_REDUCE_BLANKS               # trim meaningless whitespace
setopt INTERACTIVE_COMMENTS             # allow comments in interactive shells
setopt LIST_ROWS_FIRST                  # lay out matches horizontally
setopt LONG_LIST_JOBS                   # print job notifications in long format
#setopt PROMPT_SUBST                     # perform substitution in prompt
setopt PUSHD_IGNORE_DUPS                # don't push mulitple copies of same dir
setopt PUSHD_SILENT                     # don't print dirstack on pushd / popd
setopt PUSHD_TO_HOME                    # pushd with no args act like pushd $HOME
setopt TRANSIENT_RPROMPT                # display rprompt only on current line

PROMPT=' %%> '
RPROMPT="%~"

autoload -Uz compinit && compinit   # load compinit
zstyle ':completion:*' rehash true  # persistent rehash
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'   # match case insensitively

export EDITOR='nvim'
export VISUAL="$EDITOR"
export ALTERNATE_EDITOR=''          # start emacs --daemon if not running

flags="\
    -Wall -Wextra -Werror \
    -Wduplicated-cond -Wduplicated-branches -Wlogical-op \
    -Wrestrict -Wnull-dereference \
    -Wdouble-promotion -Wconversion -Wsign-conversion \
    -Wshadow -Wformat=2 -g \
    -fsanitize=address \
    "
export CFLAGS="-std=c11 $flags -Wjump-misses-init -Wstrict-prototypes -pedantic"
export CXXFLAGS="-std=c++17 $flags -Wold-style-cast -Wuseless-cast -Wzero-as-null-pointer-constant"
export LDFLAGS="-lm -fsanitize=address"
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
    LESS="-R +Gg"                   \
    LESS_TERMCAP_md=$'\e[1;34m'     \
    LESS_TERMCAP_us=$'\e[93m'       \
    LESS_TERMCAP_so=$'\e[01;97;40m' \
    LESS_TERMCAP_me=$'\e[0m'        \
    LESS_TERMCAP_se=$'\e[0m'        \
    LESS_TERMCAP_ue=$'\e[0m'        \
    LESS_TERMCAP_mb=$'\e[1;36m'     \
    command man "$@"
}


# aliases
alias reload='. $HOME/.zshrc'           # source .zshrc with reload command


if [ -t 0 ]; then                       # fd 0 - stdin
    stty sane
    stty -ixon                          # fuck control flow
fi

if command -v dircolors &> /dev/null; then
    eval $(dircolors -b)                # set LS_COLORS
fi

alias sudo='sudo '                      # expand aliases when using with sudo

case "$TERM" in                         # for midnight commander
    *256* )
        export COLORTERM=truecolor
        ;;
esac

mpv() {
    command mpv $@ &> /dev/null & disown
}
for cmd in cal diff egrep grep ls; do
    alias $cmd="$cmd --color=auto"
done
unset cmd
