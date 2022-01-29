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
# make caps an additional escape or control
# 2 shifts at the same time act as caps
# enable termination of x with ctrl+alt+backspace
setxkbmap                           \
    -layout 'pl,us'                 \
    -option 'caps:escape'           \
    -option 'compose:menu'          \
    -option 'shift:both_capslock'   \
    -option 'terminate:ctrl_alt_bksp'

xss-lock slock  &

# fix java programs in tiling wm
export _JAVA_AWT_WM_NONREPARENTING=1
exec qtile start
