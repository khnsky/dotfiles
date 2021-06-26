#!/bin/sh

# directory the script resides in - the root directory of the repo
SCRIPT_DIR=$(dirname -- "$0")

if ! cd -- "$SCRIPT_DIR"; then
    echo 'entering script dir failed' 1>&2
    exit 1
fi

# if no arguments were provided use all files in current directory
if [ $# -eq 0 ]; then
    set -- *
fi

echo "installing: $*"

install() {
    echo "=> installing $1"
    # is install script exists run that, else stow to home
    if [ -x "$1/install.sh" ]; then
        "$1"/install.sh
    else
        stow -t "$HOME" "$1"
    fi
}

for package; do
    # if package is not a directory skip
    [ ! -d "$package" ] && continue

    if ! install "$package"; then
        echo "$package: install failed ($?)" 1>&2
    fi
done
unset package
