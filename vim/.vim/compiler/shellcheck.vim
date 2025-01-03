if exists('current_compiler') || &compatible || !has('patch-7.4.191')
    finish
endif
let current_compiler = 'shellcheck'

if     exists('b:is_bash')
    CompilerSet makeprg=shellcheck\ -f\ gcc\ -s\ bash\ --\ %:S
elseif exists('b:is_kornshell')
    CompilerSet makeprg=shellcheck\ -f\ gcc\ -s\ ksh\ --\ %:S
else
    CompilerSet makeprg=shellcheck\ -f\ gcc\ -s\ sh\ --\ %:S
endif

CompilerSet errorformat=%f:%l:%c:\ %m\ [SC%n]
