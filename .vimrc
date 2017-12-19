" vim modeline for this file
" vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

" Piotr Kochański's .vimrc

" undos, tabs, usr_05.txt usr_toc.txt 5.05, :options

" behaviour {{{
set nocompatible            " turn vi compability off
filetype indent plugin on   " enable file dependant plugins and indenting
syntax on                   " enable syntax dependant settings

"set showmode               " default value

set visualbell

set autoindent              " copy indent form current line when making newline
set smarttab                " sw at the start of a line, sts everywhere else

set cmdheight=1             " number of lines to use for the command-line
set backspace=2             " backspace over everything in insert mode

set hidden                  " allow opening new buffers without saving changes?
" maybe set autoread & autowrite, nohidden, undo & undofile 
" and change workflow to not worring about saving
set undofile                " peristant undo history kept in a file

set ignorecase              " case insensitive search by default
set smartcase               " case sensitive search when using capital letters
set incsearch               " search as chars are entered
set hlsearch                " higlight all matches

set confirm                 " show dialog on quiting without savind etc

set wrap                    " wrap lines too long to display
set linebreak               " break at characters in breakat, not middle of a word
set breakindent             " keep indentation when wrapping

set nolazyredraw            " had problems with it off using tiling wm

set showmatch               " higlight matching parenthesis-like characters

set foldmethod=syntax       " folding based on syntax
set nofoldenable            " no folding by default

set makeprg=make            " program to use with :make command

set mouse=a                 " use mouse in all modes, maybe set to nvi, also how does copying change when setting this?
set nomousefocus            " window on which mouse is is not automaticaly activated
set mousemodel=popup        " right mouse button doesn't extend selection but instead pops up a menu?

set scrolljump=1            " lines scrolled when cursor moved off screen
set scrolloff=1             " scroll before this amout of lines off screen

set splitbelow              " put split below current window
set splitright              " put vertical split to the right

" default timeout on mappings, no timeout on escape sequences
" no lag when exiting visual mode
set timeout timeoutlen=1000 ttimeoutlen=0
" }}}

" bindings {{{
" move verticaly by viusal line
nnoremap j gj
nnoremap k gk

" retain visual selection after indenting
vnoremap > >gv
vnoremap < <gv
" }}}

" interface {{{
set number                      " show current line number
set relativenumber              " show relative line numbers for other lines
set numberwidth=4

set showcmd                     " show keys in a current chord

set list                        " show invisibles
set listchars=tab:▸\ ,eol:¬     " show tabs and eols as such

" indentation
set tabstop=4                   " visual width of <tab>
set softtabstop=4               " amount of columns tab key inserts (combination of spaces and tabs if needed)
set shiftwidth=4                " indent width
set expandtab                   " insert spaces in place of tabs

set path+=**

set wildmenu                    " show possible matches above command line on pressing wildchar
set wildmode=longest:list,full  " complete longest common string, then each full match

" terminal option, 256 colors
set t_Co=256

set bg=dark
colorscheme default

" statusline
set laststatus=2 " always show statusline

" custom highlight
highlight User1 ctermbg=none ctermfg=red

set statusline=                 " clear statusline when vimrc is reloaded
set statusline+=%1*             " use User1 highlighting
" set statusline+=f:\ 
set statusline+=%t              " filename tail
set statusline+=%y              " filetype
set statusline+=%m              " modified flag
set statusline+=%r              " read only flag
set statusline+=%h              " help file flag
set statusline+=%=              " left/right separator
set statusline+=ch:\ %c         " current char
set statusline+=\ l:\ %l/%L     " current line/all lines
set statusline+=\ %P            " percent trough file
" }}}

" autocommands {{{
" use tab in Makefiles
autocmd FileType make setlocal noexpandtab
" }}}

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
