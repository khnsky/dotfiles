" see :h ft-c-syntax
let g:c_comment_strings = 1     " highlight literals in comments
let g:c_gnu             = 1     " highlight gnu specific items
let g:c_no_ansi         = 1     " don't highlight standard ansi types / constants
let g:c_no_bsd          = 1     " don't highlight BSD specific types
let g:c_space_errors    = 1     " highlight trailing ws and mixed indent

let b:undo_ftplugin .=
    \ '|unlet! g:c_gnu g:c_space_errors g:c_no_ansi g:c_no_bsd ' .
    \ 'g:c_comment_strings g:c_space_errors'
