compiler vint

if exists(':Help') == 2
    setlocal keywordprg=:Help
else
    setlocal keywordprg=:help
endif

let g:vim_indent_cont = &l:tabstop        " see :h ft-vim-indent
let b:undo_ftplugin .=
    \ '| setlocal makeprg< errorformat< keywordprg< | unlet! g:vim_indent_cont'
