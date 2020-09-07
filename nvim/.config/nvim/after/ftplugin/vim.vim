compiler vint

let g:vim_indent_cont = &l:tabstop        " see :h ft-vim-indent
let b:undo_ftplugin .=
    \ '| setlocal makeprg< errorformat< | unlet! g:vim_indent_cont'
