[ -r "$HOME/.profile" ] && emulate sh -c 'source "$HOME/.profile"'

if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
    printf 'startx? [Y/n] '
    read -r
    case ${REPLY} in
        # exec makes it logout when x is killed
        Y | y )                      exec startx > /dev/null 2>&1   ;;
        *     ) [ -z "${REPLY}" ] && exec startx > /dev/null 2>&1   ;;
    esac
fi
