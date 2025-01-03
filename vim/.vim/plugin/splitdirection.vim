if exists('g:loaded_splitdirection')
    finish
endif
let g:loaded_splitdirection = 1

let s:cpo = &cpoptions
set cpoptions&vim

" split either vertical or horizontal based on sizes of existing windows
" or open open in a new tab if there is not enough space available
function! s:split_direction(count, count1, ...) abort
    " return modifiers for direction to split in
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

    " select direction to split in
    let l:direction = s:direction(winwidth(0) > winheight(0) * 2)
    " if count was supplied (if v:count and v:count1 are different) use it
    let l:count     = a:count ==# a:count1 ? a:count : ''
    " join variable args array with spaces
    let l:args      = join(a:000, ' ')
    " if the opened buffer is the only buffer and is empty reuse the window
    let l:only      = s:is_buf_empty() && s:is_buf_only() ? '| only' : ''

    echo l:count

    execute l:direction l:count l:args l:only

    " if opened window is less than 80 width move it to a new tab
    if winwidth(0) < 80
        wincmd T
    endif

    " scroll line under cursor to the top
    normal! zt
endfunction

command -nargs=+ -range=0 -complete=command SD
  \ call s:split_direction(v:count, v:count1, <q-args>)

command -nargs=* -range=0 -complete=help Help
  \ call s:split_direction(v:count, v:count1, 'help', <q-args>)

if &keywordprg ==# ':Man'
    let &keywordprg = ':SD Man'
endif

let &cpoptions = s:cpo
