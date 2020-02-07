# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

zstyle :compinstall filename "$ZDOTDIR/.zshrc"

HISTFILE=$ZDOTDIR/.histfile
HISTSIZE=1000
SAVEHIST=1000

bindkey -v                              # vi keybinding
KEYTIMEOUT=1                            # timeout when exiting normal mode, default 40

DIRSTACKSIZE=10

PS1='> '
RPS1="%1~"

if [ -d $ZDOTDIR/zsh.d ]; then
    for zsh in $ZDOTDIR/zsh.d/*.zsh; do
        [ -r "$zsh" ] && . "$zsh"
    done
    unset zsh
fi

if [ -t 0 ]; then                       # fd 0 - stdin
    stty sane
    stty -ixon                          # fuck control flow
fi
