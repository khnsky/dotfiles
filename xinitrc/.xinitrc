#!/bin/sh

# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

userresources=$HOME/.Xresources
sysresources=/etc/X11/xinit/.Xresources

[ -f "$sysresources"  ] && xrdb -merge "$sysresources"
[ -f "$userresources" ] && xrdb -merge "$userresources"

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        # shellcheck source=/dev/null
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# settings
# set keyboard layout
# make caps an additional escape
# enable termination of x with ctrl+alt+backspace
setxkbmap                   \
    -layout pl,us           \
    -option caps:escape     \
    -option compose:menu    \
    -option terminate:ctrl_alt_bksp

# autostart 
mpd             &
redshift        &
sxhkd           &
xss-lock slock  &
eval "$(ssh-agent)"

exec bspwm
