if exists('current_compiler') || &compatible || !has('patch-7.4.191')
    finish
endif
let current_compiler = 'ksh'

CompilerSet makeprg=ksh\ -n\ --\ %:S
CompilreSet errorformat=%f:\ %l:\ %m
