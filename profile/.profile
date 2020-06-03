if [ -d $HOME/.profile.d ]; then
    for profile in $HOME/.profile.d/*.sh; do
        [ -r "$profile" ] && . "$profile"
    done
    unset profile
fi


export EDITOR='nvim'
export VISUAL="$EDITOR"
export ALTERNATE_EDITOR=''          # start emacs --daemon if not running

export LESSHISTFILE=/dev/null       # fuck .lesshst

# see: man less
# -F: automatically exit if entire file can be diplayed on the first screen
# -i: ignore case - like vim's smartcase
# -R: output 'raw' ANSI color escape sequences
# -X: disable sending termcap init and deinit strings to terminal
#     this is sometimes desireable if deinit string does something unnecessary
#     like clearing screen -- not used - I like clearling screen but might be
#     needed when using -F
# -x: use N lenght tabs - x4 - 4 space tabs
export LESS='-FiRx4'


export BROWSER=firefox
export TERMINAL=st

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

export GNUPGHOME=$HOME/.config/gnupg
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
command -v qt5ct > /dev/null 2>&1 && export QT_QPA_PLATFORMTHEME='qt5ct'
