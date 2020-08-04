" g:is_posix for correct syntax on older runtime files, 8.0.257 fixes this
if exists('b:is_posix') && !has#('patch-8.0.257')
    let g:is_posix = 1
endif

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
    \ '| unlet b:current_compiler '         .
    \ '| setlocal errorformat< makeprg<'    .
    \ '| unlet! b:is_bash b:is_kornshell b:is_posix'
