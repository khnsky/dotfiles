# see:
#   /usr/share/doc/qtile/default_config.py
# for default config

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile import extension
from libqtile.dgroups import simple_key_binder


def __terminal():
    from shutil import which
    import os

    for terminal, arguments in {
        os.getenv("TERMINAL") or "": [],
        "kitty": ["-1"],
        "st": [],
        "alacritty": [],
        "urxvt": [],
        "rxvt": [],
        "konsole": [],
        "xfce4-terminal": [],
        "gnome-terminal": [],
        "uxterm": [],
        "xterm": [],
    }.items():
        if which(terminal, os.X_OK):
            return " ".join([terminal, *arguments])


def __launcher():
    from shutil import which
    import os

    return (
        lazy.run_extension(
            extension.DmenuRun(
                dmenu_prompt="run> ",
                background="black",
                # background="#000000",
                selected_background="#808080",
                selected_foreground="black",
                # selected_foreground="#000000",
                dmenu_lines=10,
            )
        )
        if which("dmenu", os.X_OK)
        else lazy.spawncmd()
    )


MOD = "mod4"
TERMINAL = __terminal()
LAUNCHER = __launcher()


# If a window requests to be fullscreen, it is automatically fullscreened. Set
# this to false if you only want windows to be fullscreen if you ask them to
# be.
# auto_fullscreen = True

# When clicked, should the window be brought to the front or not. If this is
# set to "floating_only", only floating windows will get affected (This sets
# the X Stack Mode to Above.)
# bring_front_click = False

# If true, the cursor follows the focus as directed by the keyboard, warping to
# the center of the focused window. When switching focus between screens, If
# there are no windows in the screen, the cursor will warp to the center of the
# screen.
# cursor_warp = False

# A function which generates group binding hotkeys. It takes a single argument,
# the DGroups object, and can use that to set up dynamic key bindings.
#
# A sample implementation is available in libqtile/dgroups.py called
# simple_key_binder(), which will bind groups to mod+shift+0-10 by default.
# dgroups_key_binder = None

# A list of Rule objects which can send windows to various groups based on
# matching criteria.
# dgroups_app_rules = []

# Default settings for extensions.
# extension_defaults = {
#    font: "sans",
#    fontsize: 12,
#    padding: 3,
# }

# The default floating layout to use. This allows you to set custom floating
# rules among other things if you wish.
#
# See the configuration file for the default float_rules.
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
    ]
)

# Behavior of the _NET_ACTIVATE_WINDOW message sent by applications
#
# urgent: urgent flag is set for the window
# focus: automatically focus the window
# smart: automatically focus if the window is in the current group
# never: never automatically focus any window that requests it
# focus_on_window_activation = "smart"

# Controls whether or not focus follows the mouse around as it moves across
# windows in a layout.
follow_mouse_focus = False

# Default settings for bar widgets. Note: if the font file associated with the
# font selected here is modified while Qtile is running, Qtile may segfault
# (for details see issue #2656).
# widget_defaults = {
#    font: "sans",
#    fontsize: 12,
#    padding: 3,
# }

# Controls whether or not to automatically reconfigure screens when there are
# changes in randr output configuration.
# reconfigure_screens = True

# Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default. We choose LG3D to maximize
# irony: it is a 3D non-reparenting WM written in java that happens to be on
# java's whitelist.
wmname = "qtile"

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
# auto_minimize = True

groups = [Group(i) for i in "1234567890"]
keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # Switch between windows
    Key([MOD], "h", lazy.layout.left()),
    Key([MOD], "l", lazy.layout.right()),
    Key([MOD], "j", lazy.layout.down()),
    Key([MOD], "k", lazy.layout.up()),
    Key([MOD], "space", lazy.layout.next()),
    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([MOD, "shift"], "h", lazy.layout.shuffle_left()),
    Key([MOD, "shift"], "l", lazy.layout.shuffle_right()),
    Key([MOD, "shift"], "j", lazy.layout.shuffle_down()),
    Key([MOD, "shift"], "k", lazy.layout.shuffle_up()),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([MOD, "control"], "h", lazy.layout.grow_left()),
    Key([MOD, "control"], "l", lazy.layout.grow_right()),
    Key([MOD, "control"], "j", lazy.layout.grow_down()),
    Key([MOD, "control"], "k", lazy.layout.grow_up()),
    Key([MOD], "n", lazy.layout.normalize()),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([MOD, "shift"], "Return", lazy.layout.toggle_split()),
    Key([MOD], "Tab", lazy.screen.toggle_group()),
    Key([MOD, "control"], "r", lazy.reload_config()),
    Key([MOD, "control"], "q", lazy.shutdown()),
    Key([MOD], "Return", lazy.spawn(TERMINAL)),
    Key([MOD], "o", LAUNCHER),
    Key([MOD], "w", lazy.window.kill()),
] + [
    key
    for group in groups
    for key in [
        Key([MOD], group.name, lazy.group[group.name].toscreen()),
        Key(
            [MOD, "shift"],
            group.name,
            lazy.window.togroup(group.name, switch_group=False),
        ),
        Key(
            [MOD, "control"],
            group.name,
            lazy.window.togroup(group.name, switch_group=True),
        ),
    ]
]

layouts = [
    layout.Columns(
        border_focus_stack=["#d75f5f", "#8f3d3d"],
        border_width=1,
        boarder_on_single=True,
        margin=2,
        margin_on_single=2,
    ),
    layout.Max(),
    layout.Floating(),
]

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentLayout(),
                widget.GroupBox(),
                widget.TaskList(max_title_width=250),
                widget.Battery(),
                widget.Sep(size_percent=50),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                widget.Systray(),
                widget.QuickExit(),
            ],
            24,
        ),
    ),
]

mouse = [
    Drag(
        [MOD],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [MOD],
        "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size(),
    ),
    Click([MOD], "Button2", lazy.window.bring_to_front()),
]
