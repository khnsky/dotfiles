" taken from github.com/jusinmk/vim-syntax-extra

" TODO learn what this does
syn match cOperator         "\(<<\|>>\|[-+*/%&^|<>!=]\)="
syn match cOperator         "<<\|>>\|&&\|||\|++\|--\|->"
syn match cOperator         "[.!~*&%<>^|=,+-]"
syn match cOperator         "/[^/*=]"me=e-1
syn match cOperator         "/$"
syn match cOperator         "&&\|||"
syn match cOperator         "[][]"

" TODO don't know if i need this
syn match cDelimiters       "[();\\]"
syn match cBraces display   "[{}]"

hi def link cDelimiter Delimiter
hi def link cBraces    Delimiter
