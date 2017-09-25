set nocompatible    " be iMproved
filetype off        " required

" plugins {{{

" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
" call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
" Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
" Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" Multiple Plug commands can be written in a single line using | separators
" Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" On-demand loading
" Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
" Plug 'tpope/vim-fireplace', { 'for': 'clojure' }

" Using a non-master branch
" Plug 'rdnetto/YCM-Generator', { 'branch': 'stable' }

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
" Plug 'fatih/vim-go', { 'tag': '*' }

" Plugin options
" Plug 'nsf/gocode', { 'tag': 'v.20150303', 'rtp': 'vim' }

" Plugin outside ~/.vim/plugged with post-update hook
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

" Unmanaged plugin (manually installed and updated)
" Plug '~/my-prototype-plugin'

" my plugins:

" Plug 'tpope/vim-fugitive'

" Plug 'scrooloose/nerdtree'

" Plug 'kien/ctrlp.vim'

" Plug 'vim-syntastic/syntastic'

"Plug 'vim-airline/vim-airline'
"
"Plug 'vim-airline/vim-airline-themes'

" Initialize plugin system
" call plug#end() 

" }}}

" behaviour {{{

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
" set clipboard=unnamedplus

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
"
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
set cursorline
highlight cursorline cterm=none ctermbg=black

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

" pugin settings {{{

" vim-airline
" let g:airline_powerline_fonts = 1 "powerline fonts
" if !exists('g:airline_symbols')
"   let g:airline_symbols = {}
" endif
" let g:airline_symbols.space = "\ua0"

" }}}

" vim:foldmethod=marker:foldmarker={{{,}}}:foldlevel=0:foldenable
