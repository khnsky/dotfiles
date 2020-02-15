if has('unix')
    setlocal path+=/usr/include
    let b:undo_ftplugin .= '|setlocal path<'
endif
