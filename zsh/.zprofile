if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
    printf 'startx? [Y/n] '
    read -r answer
    answer=${answer:-y}
    case $answer in
        # exec makes it logout when x is killed
        Y | y ) unset answer; exec startx > /dev/null 2>&1  ;;
        *     ) unset answer                                ;;
    esac
fi
