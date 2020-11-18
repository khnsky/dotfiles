let b:tex_flavor = 'pdflatex'

compiler pdflatex

let b:undo_ftplugin .= 
    \ '| unlet! b:tex_flavor g:current_compiler' .
    \ '| setlocal errorformat< makeprg<'
