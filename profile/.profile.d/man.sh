# color man pages
# boredzo.org/blog/archives/2016-08-15/colorized-man-pages-understood-and-customized
# -R: output ANSI color escape seqs in 'raw' form,
# +Gg: go to the end and back to show total lines and percentage,
# represent termcap effect as:
# ..._md: begin bold - headings (bold; blue fg)
# ..._us: begin underline - proper names (bright yellow fg)
# ..._so: begin standout mode - prompt at the bottom (bold; bright white fg; black bg)
# ..._me: reset all mode (so, us, mb, md, mr)
# ..._se: reset standout mode
# ..._ue: reset underline
# ..._mb: begin blink - unused
man() {
    LESS="-R +Gg"                               \
    LESS_TERMCAP_md=$(printf '\e[1;34m')        \
    LESS_TERMCAP_us=$(printf '\e[93m')          \
    LESS_TERMCAP_so=$(printf '\e[01;97;40m')    \
    LESS_TERMCAP_me=$(printf '\e[0m')           \
    LESS_TERMCAP_se=$(printf '\e[0m')           \
    LESS_TERMCAP_ue=$(printf '\e[0m')           \
    LESS_TERMCAP_mb=$(printf '\e[1;36m')        \
    command man "$@"
}
