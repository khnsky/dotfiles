if &compatible
    finish
endif

" TODO why does @@@ show in other windows, maybe see :h 'display'?
function! s:It()
    if exists('w:unzoom')       " w: - window local
        execute w:unzoom
        unlet   w:unzoom
    else
        let w:unzoom = winrestcmd()
        wincmd |
        wincmd _
    endif
endfunction

aug zoom.vim
    au!
    au WinLeave *
        \   if exists('w:unzoom')
        \|      execute w:unzoom
        \|      unlet   w:unzoom
        \|  endif
aug END

if has('nvim')
    " TODO: apparently <Cmd> is neovim only even then probably should check
    " for its existance
    nnoremap <Plug>ZoomIt <Cmd>call <Sid>It()<CR>
else
    nnoremap <Plug>ZoomIt :call <Sid>It()<CR>
endif
