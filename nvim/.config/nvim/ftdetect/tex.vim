" don't know if I should set this in filetype.vim instead of maybe
" after/ftdetect/tex.vim
" also use set filetype or setfiletype?

" augroup is created in $VIMRUNTIME/filetype.vim
" vint: -ProhibitAutocmdWithNoGroup
au BufNewFile,BufRead *.tex	setlocal filetype=tex
