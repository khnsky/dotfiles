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

# if xcape is installed use capslock as escape if used by itself
# and act as control if used with other keys
if command -v xcape > /dev/null 2>&1; then
    xcape -e 'Caps_Lock=Escape'
    caps='ctrl_modifier'
else
    caps='escape'
fi

# settings
# set keyboard layout
# make caps an additional escape or control
# 2 shifts at the same time act as caps
# enable termination of x with ctrl+alt+backspace
setxkbmap                           \
    -layout 'pl,us'                 \
    -option "caps:$caps"            \
    -option 'compose:menu'          \
    -option 'shift:both_capslock'   \
    -option 'terminate:ctrl_alt_bksp'

unset caps;

# autostart 
mpd             &
redshift        &
sxhkd           &
xss-lock slock  &
eval "$(ssh-agent)"

exec bspwm
