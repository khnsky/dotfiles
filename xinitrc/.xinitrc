#!/bin/sh

# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
[ -f "$sysresources" ] && xrdb -merge "$sysresources"
[ -f "$sysmodmap" ] && xmodmap "$sysmodmap"
[ -f "$userresources" ] && xrdb -merge "$userresources"
[ -f "$usermodmap" ] && xmodmap "$usermodmap"

# start some nice programs 
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# settings
# set keyboard layout
# make caps an additional escape
# enable termination of x with ctrl+alt+backspace
setxkbmap \
    -layout pl,us \
    -option caps:escape \
    -option terminate:ctrl_alt_bksp

# autostart 
mpd &
redshift &
sxhkd &
# when i figure out how to have fonts set properly when using daemon
#emacs --daemon &

# start wm
exec i3
