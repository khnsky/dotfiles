if exists('g:loaded_splitdirection')
    finish
endif
let g:loaded_splitdirection = 1

function! s:split_direction(...) abort
    function! s:get_direction(vertical) abort
        echomsg 'vertical = ' a:vertical
        if a:vertical
            return 'vertical ' . (&splitright ? 'belowright' : 'aboveleft')
        else
            return               (&splitbelow ? 'botright'   : 'topleft')
        endif
    endfunction

    function! s:is_buf_empty() abort
        return bufname('%') ==# '' || line('$') == 1 && getline(1) ==# ''
    endfunction

    function! s:is_buf_only() abort
        return len(tabpagebuflist()) == 1
    endfunction

    execute
        \ s:get_direction(winwidth(0) > winheight(0) * 2)
        \ join(a:000, ' ')
        \ (s:is_buf_empty() && s:is_buf_only() ? '| only' : '')

    if winwidth(0) < 80
        windo T
    endif

    normal! zt
endfunction

command -nargs=+ -complete=command SplitDirection
    \ call s:split_direction(<q-args>)

command -nargs=+ -complete=help Help
    \ call s:split_direction('help', <q-args>)

