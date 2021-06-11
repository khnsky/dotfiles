if exists('g:loaded_splitdirection')
    finish
endif
let g:loaded_splitdirection = 1

let s:cpo = &cpoptions
set cpoptions&vim

function! s:split_direction(count, count1, ...) abort
    function! s:direction(vertical) abort
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
      \ s:direction(winwidth(0) > winheight(0) * 2)
      \ (a:count ==# a:count1 ? a:count : '')
      \ join(a:000, ' ')
      \ (s:is_buf_empty() && s:is_buf_only() ? '| only' : '')

    if winwidth(0) < 80
        wincmd T
    endif

    normal! zt
endfunction

command -nargs=+ -range=0 -complete=command SplitDirection
    \ call s:split_direction(v:count, v:count1, <q-args>)

command -nargs=+ -range=0 -complete=help Help
    \ call s:split_direction(v:count, v:count1, 'help', <q-args>)

if &keywordprg ==# ':Man'
    let &keywordprg = ':SplitDirection Man'
endif

let &cpoptions = s:cpo
