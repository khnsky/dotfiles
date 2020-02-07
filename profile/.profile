if [ -d $HOME/.profile.d ]; then
    for profile in $HOME/.profile.d/*.sh; do
        [ -r "$profile" ] && . "$profile"
    done
    unset profile
fi

#sh_sink() {
#    $1 > /dev/null 2>&1
#}
#
#sh_has() {
#    sh_sink $(command -v $1)
#}

#contains() {
#    case $1 in
#        *$2*) return 0 ;;
#        *   ) return 1 ;;
#    esac
#}

export EDITOR='nvim'
export VISUAL="$EDITOR"
export ALTERNATE_EDITOR=''          # start emacs --daemon if not running

flags="\
    -Wall -Wextra -Werror \
    -Wduplicated-cond -Wduplicated-branches -Wlogical-op \
    -Wrestrict -Wnull-dereference \
    -Wdouble-promotion -Wconversion -Wsign-conversion \
    -Wshadow -Wformat=2 -g \
    -fsanitize=address \
    "
export CFLAGS="-std=c11 $flags -Wjump-misses-init -Wstrict-prototypes -pedantic"
export CXXFLAGS="-std=c++17 $flags -Wold-style-cast -Wuseless-cast -Wzero-as-null-pointer-constant"
export LDFLAGS="-lm -fsanitize=address"
unset flags

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
command -v qt5ct > /dev/null 2>&1 && export QT_QPA_PLATFORMTHEME='qt5ct'

unset contains
