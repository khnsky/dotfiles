[ -r "$HOME/.profile" ] && emulate sh -c 'source "$HOME/.profile"'

startplasma_wayland() {
    exec /usr/lib/plasma-dbus-run-session-if-needed \
        /usr/bin/startplasma-wayland > /dev/null 2>&1
}

startplasma_x11() {
    exec startx > /dev/null 2>&1
}

if [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ]; then
    printf '1) startplasma-wayland, 2) startx, *) tty: '
    read -r
    case ${REPLY} in
        1) startplasma_wayland  ;;
        2) startplasma_x11      ;;
    esac
fi
