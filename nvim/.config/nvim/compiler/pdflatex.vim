if exists('current_compiler') || &compatible || !has('patch-7.4.191')
    finish
endif
let g:current_compiler = 'pdflatex'

CompilerSet makeprg=pdflatex\ -file-line-error\ -interaction=nonstopmode\ %:S
CompilerSet errorformat=%f:%l:\ %m
