if exists('current_compiler') || &compatible || !has('patch-7.4.191')
    finish
endif
let current_compiler = 'vint'

if has('nvim')
    CompilerSet makeprg=vint\ -s\ --enable-neovim\ --\ %:S
else
    CompilerSet makeprg=vint\ -s\ --\ %:S
endif

" taken from syntastic:
"   github.com/vim-syntastic/syntastic/blob/master/syntax_checkers/vim/vint.vim
CompilerSet errorformat=%f:%l:%c:\ %m
