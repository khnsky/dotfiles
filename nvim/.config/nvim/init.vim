" vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

" init {{{
if &compatible                  " avoid side effects if already set
    set nocompatible            " turn vi compatibility off
endif

if 1                            " has +eval
    filetype indent plugin on   " enable file dependant plugins and indenting

    au!

    aug make                    " use tab in Makefiles
        au!
        au FileType make
                    \ setlocal noexpandtab
    aug END
endif

if has('syntax') && !exists('g:syntax_on')
    syntax enable               " enable syntax dependant settings, load once

    set synmaxcol=300           " stop searching for syntax items after columns

    " see :h ft-sh-syntax
    let g:is_posix          = 1 " default shell highlighting to posix shell
    let g:sh_fold_enabled   = 7 " enable folding for everything

    " see :h ft-c-syntax
    let g:c_comment_strings = 1 " highlight strings and numbers in comments
    let g:c_space_errors    = 1 " highlight errors in spacing
endif
" init }}}

" files {{{
set autoread                    " reload file if detected to be changed
set autowriteall                " automatically write file on some commands
set nobackup                    " disable keeping backup files
set nohidden                    " disable hiding buffers
set noswapfile                  " disable swap files
set nowritebackup               " disable write backup files

set undofile                    " persistent undo history kept in a file
set undolevels=1000             " amount of undos kept

if exists('$XDG_CACHE_HOME')    " set undodir in xdg dir
    let &g:undodir = $XDG_CACHE_HOME   . '/vim/undo//'
else
    let &g:undodir = $HOME . '/.cache' . '/vim/undo//'
endif

if !isdirectory(expand(&g:undodir))
    silent! call mkdir(expand(&g:undodir), 'p', 0700)
endif

if has('path_extra')            " see :h file-searching
    set tags=./tags;,./TAGS     " search upwards for tags/TAGS file
    set path=.,,**              " search downwards when using path (find)
endif
" files }}}

" text {{{
" searching
set ignorecase                  " case insensitive search by default ...
set smartcase                   " ... unless using uppercase letters
set hlsearch                    " highlight all matches
set incsearch                   " search incrementally

" indentation
set autoindent                  " copy indent form current line when making newline
set expandtab                   " insert spaces in place of tabs
set shiftround                  " < and > indent to multiples of shiftwidth
set shiftwidth=4                " indent width
set smarttab                    " sw at the start of a line, sts everywhere else
set softtabstop=4               " amount of columns (tabs/spaces) inserted by <tab>
set tabstop=4                   " visual width of <tab>

set wrap                        " wrap lines too long to display
set linebreak                   " break lines at sensible places
set breakindent                 " keep indentation when wrapping

set foldmethod=syntax           " folding based on syntax
set nofoldenable                " no folding by default

set showmatch                   " highlight matching parenthesis-like characters

set backspace=2                 " backspace over everything in insert mode

set complete-=i                 " completion from included files is slow

set nrformats-=octal            " predictable number increasing and decreasing

if v:version > 703 || v:version == 703 && has('patch541')
    set formatoptions+=j        " remove comment leader when joining files
endif

set virtualedit=block           " allow movement over nonexistent chars in visual
" text }}}

" windows {{{
if has('mouse')
    set mouse=nvi               " use mouse in all modes
    if has('mouse_sgr')         " resize splits with mouse
        set ttymouse=sgr        " work in columns beyond 223
    elseif has('mouse_xterm')
        set ttymouse=xterm2
    endif
endif

set splitbelow                  " put split below current window
set splitright                  " put vertical split to the right

set scrolljump=10               " lines jumped when cursor moves off screen
set scrolloff=1                 " scroll before this amount of lines off screen
set sidescrolloff=5             " same as scrolloff but to the side
" windows }}}

" ui {{{
set visualbell                  " use visual bell instead of beeping

set number                      " show current line number
set relativenumber              " show relative line numbers for other lines

" statusline
set cmdheight=2                 " avoid 'press <Enter> to continue' if possible
set showcmd                     " show keys in a current chord
set laststatus=2                " always show statusline
set ruler                       " show cursor position

" dialogs
set confirm                     " show dialog on quitting without saving etc.
set wildmenu                    " show possible completions
set wildmode=longest:full,full  " complete longest common string, then each full match
" ui }}}

" mappings {{{
set ttimeout                    " timeout on key codes
set ttimeoutlen=100             " key code timeout

if has('langmap') && exists('+langremap')
    set nolangremap             " don't use langmap for chars resulting from mappings
endif

if has('nvim')                  " use <Esc> to exit terminal-mode
    tnoremap <Esc> <C-\><C-n>
endif

" break undo on <C-U>
inoremap <C-U> <C-G>u<C-U>
" break undo on newline
inoremap <CR> <C-G>u<CR>

let mapleader = '\'
map <Space> <Leader>

" use Q for formatting, there is gQ for Ex
map Q gq
" copy to EOL instead of copying line, more analogous to the rest of operators
map Y y$

" clear search highlight in addition to clearing & redrawing screen
nnoremap <C-L> :nohl<CR><C-L>
" mappings }}}

" misc. {{{
if !exists(':DiffOrig')         " see :h DiffOrig
    command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                \ | wincmd p | diffthis
endif

if &history < 1000
    set history=1000            " entries in ':' cmds and search patterns histories
endif

if &tabpagemax < 50
    set tabpagemax=50           " max nr. of tabs opened by -p arg or ':tab all'
endif

set lazyredraw                  " don't redraw while executing macros
" misc. }}}

" netrw {{{
let g:netrw_banner          = 0
let g:netrw_liststyle       = 3     " tree view
" }}}
