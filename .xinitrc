#!/bin/sh

# vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

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
# hothey daemon
# is &> notation posix compliant?
pgrep sxhkd &> /dev/null || sxhkd &> /dev/null &
# music player daemon
pgrep mpd &> /dev/null || mpd &> /dev/null &
# if redshift is not running start redshift in background, redirect stdout & stderr to /dev/null
pgrep redshift &> /dev/null || redshift &> /dev/null &

# start wm
exec i3
