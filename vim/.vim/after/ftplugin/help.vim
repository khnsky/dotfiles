if exists(':Help') == 2
    setlocal keywordprg=:Help
else
    setlocal keywordprg=:help
endif

if has('conceal') && &modifiable && !&readonly
    setlocal conceallevel=0
    let b:undo_ftplugin .= '| setlocal conceallevel<'
else
    nnoremap <buffer> q :q<cr>
    let b:undo_ftplugin .= '| nunmap <buffer> q'
endif

let b:undo_ftplugin .= '| setlocal keywordprg<'
