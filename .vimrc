" vim modeline for this file
" vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

" Piotr Kochański's .vimrc

" behaviour {{{
set nocompatible            " turn vi compability off
filetype indent plugin on   " enable file dependant plugins and indenting
syntax on                   " enable syntax dependant settings

" backspace over everything in insert mode
set backspace=2

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

set bg=dark
colorscheme default

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

" autocommands
" use tab in Makefiles
autocmd FileType make setlocal noexpandtab

" plugins {{{
" if plug.vim is not present in autoload dir download it, install plugins
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

" syntax cheking
Plug 'vim-syntastic/syntastic'

" file explorer
Plug 'scrooloose/nerdtree'

call plug#end()
" }}}

" pugin settings {{{
" syntastic settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" nerdtree
map <C-n> :NERDTreeToggle<CR>
" }}}

