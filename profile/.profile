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

# crude way to make so that everything piped to pager in nnn doesn't disappear
# immediately as it does with -F set
alias nnn='LESS= nnn'

# ignore ignoring of return value
# shellcheck disable=2155
export PAGER="$(command -v less || print 'cat')"
export MANPAGER="$PAGER" GIT_PAGER="$PAGER"

# items are listed in reverse priority because every encountered item is exported
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

export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
export XDG_STATE_HOME=${XDG_STATE_HOME:-$HOME/.local/state}

export XDG_CONFIG_DIRS=${XDG_CONFIG_DIRS:-/etc/xdg}
export XDG_DATA_DIRS=${XDG_DATA_DIRS:-/usr/local/share:/usr/share}

if [ -f "$XDG_CONFIG_HOME/user-dirs.dirs" ]; then
    . "$XDG_CONFIG_HOME/user-dirs.dirs" > /dev/null
    export                  \
        XDG_DESKTOP_DIR     \
        XDG_DOCUMENTS_DIR   \
        XDG_DOWNLOAD_DIR    \
        XDG_MUSIC_DIR       \
        XDG_PICTURES_DIR    \
        XDG_VIDEOS_DIR
fi

export CABAL_CONFIG=$XDG_CONFIG_HOME/cabal/config
export CABAL_DIR=$XDG_CACHE_HOME/cabal
export CARGO_HOME=$XDG_CONFIG_HOME/cargo
export GDBHISTFILE=$XDG_CONFIG_HOME/gdb/history
export GNUPGHOME=$XDG_CONFIG_HOME/gnupg
export GTK2_RC_FILES=$XDG_CONFIG_HOME/gtk-2.0/gtkrc
export INPUTRC=$XDG_CONFIG_HOME/readline/inputrc
export IPYTHONDIR=$XDG_CONFIG_HOME/jupyter
export JUPYTER_CONFIG_DIR=$XDG_CONFIG_HOME/jupyter
export NUGET_PACKAGES=$XDG_CACHE_HOME/NuGetPackages
export OCTAVE_HISTFILE=/dev/null
export PYLINTHOME=$XDG_CACHE_HOME/pylint
export PYTHONSTARTUP=$XDG_CONFIG_HOME/python/startup.py
export RANDFILE=$XDG_DATA_HOME/rnd
export RUSTUP_HOME=$XDG_DATA_HOME/rustup

command -v qt5ct > /dev/null 2>&1 && export QT_QPA_PLATFORMTHEME='qt5ct'

# disable telemetry
export DOTNET_CLI_TELEMETRY_OPTOUT=1

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent -t 1h > "$XDG_RUNTIME_DIR/ssh-agent.env"
fi
if [ -z "$SSH_AUTH_SOCK" ]; then
    # shellcheck source=/dev/null
    . "$XDG_RUNTIME_DIR/ssh-agent.env" > /dev/null
fi

# prepend systemd file-hierarchy directory for user-local binaries to $PATH
# only prepend if not already there
case ":$PATH:" in
    *:$HOME/.local/bin:*)
        ;;
    *)
        export PATH="$HOME/.local/bin:$PATH"
esac
