setlocal commentstring&vim define&vim include&vim
let b:undo_ftplugin .= '|setlocal commentstring< define< include<'

setlocal complete+=d
let b:undo_ftplugin .= '|setlocal complete<'

let &l:makeprg =
    \   'if [ -r Makefile ] \|\| [ -r makefile ]; then' .
    \   '  make $*;'                                    .
    \   'else'                                          .
    \   '  make %:r:S;'                                 .
    \   'fi'

"   setlocal option< copies global setting instead of unsetting local option
"   is this what is supposed to happen here
"
"   for string options setlocal< copies the global value, setlocal= and set<
"   clears the local value so that global value is used instead
let b:undo_ftplugin .= '|setlocal makeprg<'

if has('unix')
    setlocal path+=/usr/include
    let b:undo_ftplugin .= '|setlocal path<'
endif

" see :h ft-c-syntax
let g:c_comment_strings = 1     " highlight literals in comments
let g:c_gnu             = 1     " highlight gnu specific items
let g:c_no_ansi         = 1     " don't highlight standard ansi types / constants
let g:c_no_bsd          = 1     " don't highlight BSD specific types
let g:c_space_errors    = 1     " highlight trailing ws and mixed indent

let b:undo_ftplugin .=
    \ '|unlet! g:c_gnu g:c_space_errors g:c_no_ansi g:c_no_bsd' .
    \ 'g:c_comment_strings g:c_space_errors'
