if [ -f ~/.config/pacman/pacman.conf ]; then
    alias pacman='pacman --config ~/.config/pacman/pacman.conf'
    command -v yay > /dev/null 2>&1 && yay() {
        if [ $# -ne 0 ]; then
            command yay --config ~/.config/pacman/pacman.conf "$@"
        else
            command yay --config ~/.config/pacman/pacman.conf -Syu
        fi
    }
fi
