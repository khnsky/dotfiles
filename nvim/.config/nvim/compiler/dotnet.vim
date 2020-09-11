if exists('current_compiler') || &compatible || !has('patch-7.4.191')
    finish
endif
let current_compiler = 'dotnet'

CompilerSet makeprg=dotnet\ build\ /p:GenerateFullPaths=true
CompilerSet errorformat=
    \%-A%.%#Microsoft%.%#,
    \%-ZBuild\ FAILED.,
    \%C%.%#,
    \%f(%l\\\,%c):\ %tarning\ %m\ [%.%#],
    \%f(%l\\\,%c):\ %trror\ %m\ [%.%#],
    \%-G%.%#
