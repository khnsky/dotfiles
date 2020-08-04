if exists('b:did_ftplugin')
    finish
endif

runtime! ftpluing/c.vim ftplugin/c_*.vim ftplugin/c/*.vim
let b:did_ftplugin = 1

let &l:makeprg =
    \   'if [ -r Makefile ] \|\| [ -r makefile ]; then' .
    \   '  make $*;'                                    .
    \   'else'                                          .
    \   '  make %:r:S;'                                 .
    \   'fi'

let b:undo_ftplugin  = get(b:, 'undo_ftplugin', '')
let b:undo_ftplugin .= '| setlocal makeprg<'

