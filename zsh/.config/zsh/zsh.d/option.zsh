setopt AUTO_CD                          # cd if not a command and dir exist
setopt AUTO_PUSHD                       # add dir to dirstack on cd
setopt COMPLETE_ALIASES                 # alias completion before substitution
setopt CORRECT_ALL
setopt EXTENDED_GLOB                    # use extended globs, may be unexpected
setopt HIST_IGNORE_DUPS                 # don't save consecutive duplicate commands
setopt HIST_IGNORE_SPACE                # don't save commands preceded with space
setopt HIST_REDUCE_BLANKS               # trim meaningless whitespace
setopt INTERACTIVE_COMMENTS             # allow comments in interactive shells
setopt LIST_ROWS_FIRST                  # lay out matches horizontally
setopt LONG_LIST_JOBS                   # print job notifications in long format
setopt PUSHD_IGNORE_DUPS                # don't push mulitple copies of same dir
setopt PUSHD_SILENT                     # don't print dirstack on pushd / popd
setopt PUSHD_TO_HOME                    # pushd with no args act like pushd $HOME
setopt TRANSIENT_RPROMPT                # display rprompt only on current line
