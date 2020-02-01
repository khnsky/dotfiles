#!/bin/sh

WD=$(pwd)
DIR=$(dirname "$0")

if ! cd "$DIR"; then
    echo 'entering script dir failed' 1>&2
    exit 1
fi

FILES=${*:-*}

echo "installing: $FILES"

for f in $FILES; do
    [ ! -d "$f" ] && continue

    echo "=> installing $f"
    if [ -x "$f/install.sh" ]; then
        "$f/install.sh"
    else
        stow -d "$DIR" -t "$HOME" "$f"
    fi

    [ $? -ne 0 ] && echo "$f install failed ($?)" 1>&2
done

if ! cd "$WD"; then
    echo 'returning to working directory failed' 1>&2
    exit 1
fi
