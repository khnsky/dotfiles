if exists('current_compiler') || &compatible || !has('patch-7.4.191')
    finish
endif
let current_compiler = 'bash'

CompilerSet makeprg=bash\ -n\ --\ %:S
CompilerSet errorformat=%f:\ lline\ %l:\ %m
