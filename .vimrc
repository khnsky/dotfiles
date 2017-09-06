set nocompatible	" be iMproved
filetype off		" required

" ----- PLUGINS ----- {{{
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

" ----- WORKFLOW ----- {{{
" enable file dependant plugins and indenting
filetype indent plugin on

" enable syntax
syntax on

" enable hybrid numbering and number width of 4
set number relativenumber numberwidth=4

" make commands show in right down corner
set showcmd

" show end of line and tab chars (invisibles)
set list listchars=tab:▸\ ,eol:¬

" set tabs to 4 spaces length and use tabs not spaces
set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab

"
set hidden

" visual autocomplete
set wildmenu

" search case insenitive unless cap. letter used or \C flag
set ignorecase smartcase

" display confiramtion prompt instead of error?
set confirm

" wrap lines in sensible places, keep indentation
set wrap linebreak breakindent
" use system CLIPBOARD buffer
set clipboard=unnamedplus

" redraw after new window is opened in i3
set nolazyredraw

" higlight matching parenthesis-like characters
set showmatch

" search as characters are entered and highlight matches
set incsearch hlsearch
" always show statusline
set laststatus=2

" folding based on syntax, not folded on default
set foldmethod=syntax nofoldenable
" }}}

" ----- INTERFACE ----- {{{
set t_Co=256
colorscheme default

" highlight current line
set cursorline
highlight cursorline cterm=none ctermbg=black
" }}}

" ----- PLUGIN SETTINGS ----- {{{
" vim-airline


" let g:airline_powerline_fonts = 1 "powerline fonts
" if !exists('g:airline_symbols')
" 	let g:airline_symbols = {}
" endif
" let g:airline_symbols.space = "\ua0"
" }}}

" vim:foldmethod=marker:foldmarker={{{,}}}:foldlevel=0
