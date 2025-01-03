if exists('g:loaded_directory')
    finish
endif
let g:loaded_directory = 1

function! s:sort(lhs, rhs) abort
    if a:lhs[-1:] ==# '/' && a:rhs[-1:] !=# '/'
        return -1
    elseif a:lhs[-1:] !=# '/' && a:rhs[-1:] ==# '/'
        return 1
    endif
    if a:lhs < a:rhs
        return -1
    elseif a:lhs > a:rhs
        return 1
    endif
    return 0
endfunction

function! s:ls(path) abort
    return
      \ sort(
      \     filter(
      \         extend(
      \             glob(a:path . '/.*', v:true, v:true),
      \             glob(a:path . '/*',  v:true, v:true),
      \         ),
      \         'v:true'
      \     ),
      \     function('s:sort')
      \ )
endfunction

function! s:populate() abort
    let l:path = resolve(expand('%:~:.'))
    if !isdirectory(l:path)
        return
    endif

    " see :h special-buffers
    setlocal buftype=nowrite bufhidden=wipe noswapfile modifiable
    silent keepmarks keepjumps call setline(1, s:ls(l:path))
    setlocal nomodified nomodifiable
endfunction

function! s:qfls() abort
    let l:lines = s:ls(expand('%:~:.:h'))
    call setqflist([], ' ', { 'lines': l:lines, 'efm': '%f' })
    copen
endfunction

command QFls call s:qfls()
nnoremap <silent> <plug>(qf-ls) :<c-u>QFls<cr>

augroup directory
    autocmd!
    autocmd BufEnter * call s:populate()
augroup END
