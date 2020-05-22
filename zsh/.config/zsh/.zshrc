# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

zstyle :compinstall filename "$ZDOTDIR/.zshrc"

HISTFILE=$ZDOTDIR/.histfile
HISTSIZE=1000
SAVEHIST=1000

bindkey -v                              # vi keybinding
KEYTIMEOUT=1                            # timeout for interpreting esc codes

DIRSTACKSIZE=10

# this is not a perfect solution - there could be some isses when using
# multiline prompts etc.
# see:  unix.stackexchange.com/q/547
#       man zshzle
zle-keymap-select zle-line-init() {
    case $KEYMAP in
        vicmd)      print -n '\1\e[2 q\2';;
        viins|main) print -n '\1\e[6 q\2';;
    esac
}
zle -N zle-keymap-select
zle -N zle-line-init

PS1='> '
RPS1="%1~"

if [ -d $ZDOTDIR/zsh.d ]; then
    for zsh in $ZDOTDIR/zsh.d/*.zsh; do
        [ -r "$zsh" ] && . "$zsh"
    done
    unset zsh
fi

# https://wiki.archlinux.org/index.php/Bash/Functions#Display_error_codes
ec() {
    echo -e '\e[1;33m'code $?'\e[m\n'
}
trap ec ERR

if [ -t 0 ]; then                       # fd 0 - stdin
    stty sane
    stty -ixon                          # fuck control flow
fi
