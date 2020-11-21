if [ -d "$HOME"/.profile.d ]; then
    for profile in "$HOME"/.profile.d/*.sh; do
        # shellcheck source=/dev/null
        [ -r "$profile" ] && . "$profile"
    done
    unset profile
fi

export HISTFILE="$HOME/.history"
export HISTSIZE=2000
export SAVEHIST=$HISTSIZE

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

# ignore ignoring of return value
# shellcheck disable=2155
export PAGER="$(command -v less || print 'cat')"
export MANPAGER="$PAGER" GIT_PAGER="$PAGER"

for i in xterm urxvt gnome-terminal xfce4-terminal konsole alacrity st kitty; do
    command -v $i > /dev/null 2>&1 && export TERMINAL=$i
done

for i in chrome chromium firefox icecat; do
    command -v $i > /dev/null 2>&1 && export BROWSER=$i
done

for i in cc gcc clang; do
    command -v $i > /dev/null 2>&1 && export CC=$i
done

for i in cpp g++ clang++; do
    command -v $i > /dev/null 2>&1 && export CXX=$i
done
unset i

export CFLAGS='-std=c11 -Wall -Wextra -Werror -g -pedantic'
export CXXFLAGS='-std=c++17 -Wall -Wextra -Werror -g'
export LDFLAGS='-lm'

export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export GNUPGHOME=$XDG_CONFIG_HOME/gnupg
command -v qt5ct > /dev/null 2>&1 && export QT_QPA_PLATFORMTHEME='qt5ct'

# disable telemetry
export DOTNET_CLI_TELEMETRY_OPTOUT=1
