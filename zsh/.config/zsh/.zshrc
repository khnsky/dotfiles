# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

# check if stdin is connected to a terminal - interactive, else return
[ ! -t 0 ] && return

zstyle :compinstall filename "$ZDOTDIR/.zshrc"

bindkey -v                              # vi keybinding
KEYTIMEOUT=1                            # timeout for interpreting esc codes

DIRSTACKSIZE=10

# change cursor shape based on mode
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

# wiki.archlinux.org/index.php/Bash/Functions#Display_error_codes
ec() {
    echo -e '\e[1;31m$? = '" $?"'\e[m\n'
}
trap ec ERR

# fuck control flow
stty sane
stty -ixon

if [ -d $ZDOTDIR/zsh.d ]; then
    for zsh in $ZDOTDIR/zsh.d/*.zsh; do
        [ -r "$zsh" ] && . "$zsh"
    done
    unset zsh
fi

# tmux is not already running and not root
if [ -z "$TMUX" ] && [ "$UID" != 0 ]; then
    # -A acts as attach session if `session-name` already exists, since v. 1.8
    command -v tmux && tmux new-session -As "$HOST"
fi
