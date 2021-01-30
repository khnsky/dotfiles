compiler vint

if exists(':Help') == 2
    setlocal keywordprg=:Help
else
    setlocal keywordprg=:help
endif

" see :h ft-vim-indent
"  -2 for backslash and a space so everything aligns
let g:vim_indent_cont = &l:tabstop > 2 ? &l:tabstop - 2 : &l:tabstop
let b:undo_ftplugin .=
    \ '| setlocal makeprg< errorformat< keywordprg< | unlet! g:vim_indent_cont'
