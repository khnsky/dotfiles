if exists('g:loaded_matchit') || &compatible
    finish
endif

if has('packages') && !has('nvim')      " nvim loads matchit automatically
    packadd matchit                     " don't use ! because already in plugin
else
    runtime macros/matchit.vim
endif
