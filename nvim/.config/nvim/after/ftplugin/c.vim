setlocal commentstring&vim define&vim include&vim
let b:undo_ftplugin .= '|setlocal commentstring< define< include<'

setlocal complete+=d
let b:undo_ftplugin .= '|setlocal complete<'

if has('unix')
    setlocal path+=/usr/include
    let b:undo_ftplugin .= '|setlocal path<'
endif
