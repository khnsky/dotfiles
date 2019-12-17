DIR=$(dirname $0)
FILES=${@:-*}

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
unset f
