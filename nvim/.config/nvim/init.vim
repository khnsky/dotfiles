" vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

" behaviour {{{
set nocompatible            " turn vi compatibility off
filetype indent plugin on   " enable file dependant plugins and indenting
syntax on                   " enable syntax dependant settings

set visualbell

set autoindent              " copy indent form current line when making newline
set smarttab                " sw at the start of a line, sts everywhere else

set backspace=2             " backspace over everything in insert mode

set hidden                  " allow opening new buffers without saving changes?
" maybe set autoread & autowrite, nohidden, undo & undofile 
" and change workflow to not worring about saving
set undofile                " persistent undo history kept in a file

set ignorecase              " case insensitive search by default
set smartcase               " case sensitive search when using capital letters
set incsearch               " search as chars are entered
set hlsearch                " highlight all matches

set confirm                 " show dialog on quitting without saving etc

set wrap                    " wrap lines too long to display
set linebreak               " break at characters in breakat, not middle of a word
set breakindent             " keep indentation when wrapping

set showmatch               " highlight matching parenthesis-like characters

set foldmethod=syntax       " folding based on syntax
set nofoldenable            " no folding by default

set mouse=a                 " use mouse in all modes, maybe set to nvi, also how does copying change when setting this?
if has("mouse_sgr")
    set ttymouse=sgr
else
    set ttymouse=xterm2
end

set scrolljump=1            " lines scrolled when cursor moved off screen
set scrolloff=1             " scroll before this amount of lines off screen

set splitbelow              " put split below current window
set splitright              " put vertical split to the right

" default timeout on mappings, no timeout on escape sequences
" no lag when exiting visual mode
set timeout timeoutlen=1000 ttimeoutlen=0

set tags=./tags;./TAGS          " recursively look up for tags or TAGS file
" }}}

" interface {{{
set number                      " show current line number
set relativenumber              " show relative line numbers for other lines

set showcmd                     " show keys in a current chord

" indentation
set tabstop=4                   " visual width of <tab>
set softtabstop=4               " amount of columns tab key inserts (combination of spaces and tabs if needed)
set shiftwidth=4                " indent width
set expandtab                   " insert spaces in place of tabs
set shiftround                  " < and > indent to multiples of shiftwidth

set path=.,,**

set wildmenu                    " show possible matches above command line on pressing wildchar
set wildmode=longest:full,full  " complete longest common string, then each full match

set laststatus=2                " always show statusline
" }}}

" autocommands {{{
" use tab in Makefiles
autocmd FileType make setlocal noexpandtab
" }}}
