import atexit, os

# python stores command history in `~/.python_history` littering in your $HOME
# the only way to change that behaviour seems to provide your own
# sys.__interactivehook__ but then you would lose any future update without
# manual intervention.  because I have no use for this file I choose to simply
# delete it.
#
# functions registered using atexit are run in reverse order from the one they
# were registered in, thus since this is registered before
# sys.__interactivehook__ is run this will be run after.
atexit.register(
    lambda: os.remove(os.path.join(os.path.expanduser("~"), ".python_history"))
)
