if exists(':DiffOrig')                  " see :h DiffOrig
    finish
endif

command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
    \ | wincmd p | diffthis
