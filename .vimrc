" vim modeline for this file
" vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

" Piotr Kochański's .vimrc

" behaviour {{{
" turn vi compability off
set nocompatible

" enable file dependant plugins and indenting
filetype indent plugin on

" enable syntax dependant settings
syntax on

" allow opening new buffers without saving changes?
set hidden

" case insensitive search
" unless using capital letters
" or specifing \C flag
set ignorecase smartcase

" show dialog when action needs confirmation
" quitind without saving, overwritting read only file, etc
set confirm

" wrap lines too long to display
" break lines at characters specified in breakat instead of middle of word
" keep indentation on wraps
set wrap linebreak breakindent

" use system CLIPBOARD buffer
"set clipboard=unnamedplus

" redraw needed when terminal size changes?
set nolazyredraw

" higlight matching parenthesis-like characters
set showmatch

" search as characters are entered and highlight matches
set incsearch hlsearch
"
" folding based on syntax, not folded on default
set foldmethod=syntax nofoldenable

" default timeout on mappings, no timeout on escape sequences
" no lag when exiting visual mode
set timeout timeoutlen=1000 ttimeoutlen=0
" }}}

" interface {{{
" show line number on current line
" and relative line numbering for others
" width of line numbers 4
set number relativenumber numberwidth=4

" show pressed keys in down right corner
set showcmd

" show end of line and tab chars (invisibles) as characters after colon
set list listchars=tab:▸\ ,eol:¬

" indentation
set tabstop=4       " width of <tab>
set softtabstop=4   " amount of columns tab key inserts (combination of spaces and tabs if needed)
set shiftwidth=4    " indent width
set expandtab       " insert spaces in place of tabs

" enhanced comand line autocompletition
set wildmenu

" terminal option, 256 colors
set t_Co=256

colorscheme default

" highlight current line
"set cursorline
"highlight cursorline cterm=none ctermbg=black

" statusline
" always show statusline
set laststatus=2

" custom highlight
highlight User1 ctermbg=none ctermfg=red

set statusline=         " clear statusline when vimrc is reloaded
set statusline+=%1*     " use User1 highlighting
set statusline+=%t      " filename tail
set statusline+=%y      " filetype
set statusline+=%m      " modified flag
set statusline+=%r      " read only flag
set statusline+=%h      " help file flag
set statusline+=%=      " left/right separator
set statusline+=%l/%L   " current line/all lines
set statusline+=\ %P    " percent trough file
" }}}

" plugins {{{
"if has('nvim')
"    if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
"      silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
"        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"      autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
"    endif
"else
"    if empty(glob('~/.vim/autoload/plug.vim'))
"      silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
"        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
"      autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
"    endif
"endif

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'vim-syntastic/syntastic'

Plug 'scrooloose/nerdtree'

call plug#end()
" }}}

" pugin settings {{{
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" }}}

