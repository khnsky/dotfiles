#!/bin/sh

update()
{
    pacman -Qqe > pkglist
}

install()
{
    sudo pacman -S < pkglist
}

usage()
{
    echo "unknown or no argument
    usage:
            update to refresh pkglist
            install to install packages in current pkglist"
}

# check for privilidge?

case "$1" in
    "update") update;;
    "install") install;;
    *) usage;;
esac

exit 1
