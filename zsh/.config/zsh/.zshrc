# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

# check if stdin is connected to a terminal - interactive, else return
[ ! -t 0 ] && return

zstyle :compinstall filename "$ZDOTDIR/.zshrc"

bindkey -v                              # vi keybinding
KEYTIMEOUT=1                            # timeout for interpreting esc codes

# ^[[Z is shift-tab - navigate completion backwards with shift-tab
bindkey '^[[Z' reverse-menu-complete

DIRSTACKSIZE=10

# change cursor shape based on mode
# this is not a perfect solution - there could be some isses when using
# multiline prompts etc.
# see:  unix.stackexchange.com/q/547
#       man zshzle
zle-keymap-select zle-line-init() {
    case $KEYMAP in
        vicmd)          print -n '\1\e[2 q\2';;
        viins | main)   print -n '\1\e[6 q\2';;
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

# see:
#   https://www.reddit.com/r/vim/comments/9bm3x0/ctrlz_binding/
#   https://blog.sher.pl/2014/03/21/how-to-boost-your-vim-productivity/
# run fg preserving current command line (push input)
ctrl-z() {
    # push input onto buffer stack and return to top level prompt
    # next time editor starts construct will be popped
    zle push-input
    # set buffer to fg
    # don't know why this is needed instead of just `fg` but this works better
    # otherwise ressume message is put on the prompt after exiting/suspending
    # resumed command
    BUFFER="fg"
    # finish editing the buffer - this normally causes it to be exectuted
    # like a shell command
    zle accept-line
}
zle -N ctrl-z
bindkey '^Z' ctrl-z
HISTORY_IGNORE='(history|fg)'

# tmux is not already running and not root
if [ -z "$TMUX" ] && [ "$UID" != 0 ]; then
    # -A acts as attach session if `session-name` already exists, since v. 1.8
    command -v tmux && tmux new-session -As "$HOST"
fi
