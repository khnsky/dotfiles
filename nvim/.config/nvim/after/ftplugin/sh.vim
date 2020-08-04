if executable('shellcheck')
    compiler shellcheck
elseif exists('b:is_bash')
    compiler bash
elseif exists('b:is_kornshell')
    compiler ksh
else
    compiler sh
endif

let b:undo_ftplugin .=
    \ '| unlet b:current_compiler ' .
    \ '| setlocal errorformat< makeprg<'
