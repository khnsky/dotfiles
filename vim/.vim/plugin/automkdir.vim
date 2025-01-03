function! s:AutoMkDir() abort
    let l:dir = expand('%:h')
    let l:msg = 'Directory ' . l:dir . ' doesn"t exist. Create it?'
    if !isdirectory(l:dir) && confirm(l:msg, "&Yes\n&No") == 1
        call mkdir(l:dir, 'p')
        echomsg 'Created directory' l:dir
    endif
endfunction

aug automkdir
    au!
    " redraw! to avoid hit-enter prompt after creating directory
    au BufWritePre,FileWritePre * call s:AutoMkDir() | redraw!
aug END
