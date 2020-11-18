if command -v dircolors > /dev/null 2>&1; then
    if [ -f ~/.dir_colors ]; then
        eval "$(dircolors -b ~/.dir_colors)"
    elif [ -f /etc/DIR_COLORS ]; then
        eval "$(dircolors -b /etc/DIR_COLORS)"
    else
        eval "$(dircolors -b)"
    fi
fi

case "$TERM" in                         # for midnight commander
    *256*) export COLORTERM="${COLORTERM:-truecolor}" ;;
esac

for cmd in cal diff egrep grep ls; do
    # expands when defined not when used - as intended
    # shellcheck disable=2139
    alias $cmd="$cmd --color=auto"
done
unset cmd
