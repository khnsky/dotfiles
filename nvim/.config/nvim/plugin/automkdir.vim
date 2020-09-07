" TODO: solve hit-enter problem

function! s:AutoMkDir() abort
    let l:dir = expand('%:h')
    if !isdirectory(l:dir) && confirm(
        \ 'Directory ' . l:dir . ' doesn"t exist. Create it?',
        \ "&Yes\n&No") == 1
        call mkdir(l:dir, 'p')
        echomsg 'Created directory' l:dir
    endif
endfunction

aug automkdir
    au!
    au BufWritePre * call s:AutoMkDir()
aug END
