" vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

" init {{{
if &compatible                          " avoid side effects if already set
    set nocompatible                    " turn vi compatibility off
endif

if 1                                    " has +eval
    filetype indent plugin on           " enable file type plugins and indent

    aug vimrc
        au!
        " use tabs to indent in Makefiles
        au FileType make
                    \   setl noexpandtab
        " :f, gf etc. works for include files
        au FileType c,cpp
                    \   setl path+=/usr/include

        " set foldlevel to a minimal value while having all folds still open
        " foldlevel is buffer local, use foldlevelstart for initial foldlevel
        " value, however zm wont close folds right away if it is set too high
        if has('folding')
            au BufRead *
                    \   exe &foldenable
                    \       ? 'normal! zR'
                    \       : 'setl fdc=0'
        endif

        au Colorscheme desert call OverwriteColorscheme()
    aug END

    func! OverwriteColorscheme()
        hi CursorLineNr ctermfg=White
        hi LineNr       ctermfg=DarkGrey
        hi StatusLine   cterm=None          ctermfg=LightGrey
        hi StatusLineNC cterm=None          ctermfg=DarkGrey
        hi TabLine      cterm=None          ctermfg=DarkGrey    ctermbg=None
        hi TabLineFill  cterm=None
        hi TabLineSel   cterm=None          ctermfg=White
        hi VertSplit    cterm=None          ctermfg=DarkGrey
        hi WildMenu     cterm=None          ctermfg=White       ctermbg=None
        hi Visual       cterm=Bold,Reverse  ctermfg=None        ctermbg=None
    endfunc
endif

if has('syntax') && !exists('g:syntax_on')
    syntax enable                       " load syntax highlight once
    colorscheme desert

    set synmaxcol=300                   " stop searching for syntax items after

    " see :h ft-sh-syntax
    let g:is_posix          = 1         " use posix shell syntax highlight
    let g:sh_fold_enabled   = 7         " enable folding for everything

    " see :h ft-c-syntax
    let g:c_comment_strings = 1         " highlight literals in comments
    let g:c_space_errors    = 1         " highlight errors in spacing
endif
" init }}}

" files {{{
set autoread                            " reload file if detected to be changed
set autowriteall                        " some commands force :write
set nobackup                            " disable keeping backup files
set nohidden                            " disable hiding buffers
set noswapfile                          " disable swap files
set nowritebackup                       " disable write backup files

if has('persistent_undo')               " persistent undo history
    set undofile

    " use xdg cache dir if possible to store undofiles
    if exists('$XDG_CACHE_HOME')
        let &g:undodir = $XDG_CACHE_HOME   . '/vim/undo//'
    else
        let &g:undodir = $HOME . '/.cache' . '/vim/undo//'
    endif

    if !isdirectory(expand(&g:undodir))
        silent! call mkdir(expand(&g:undodir), 'p', 0700)
    endif
endif

" TODO
if has('path_extra')                    " see :h file-searching
    set path=.,,**                      " search down when using 'path' (:find)
    set tags=./tags; tags+=tags         " search for tags in current dir and up in dir of current file
    if has('emacs_tags')
        set tags+=./TAGS; tags+=TAGS    
    endif
endif

set tagcase=match                       " match tags case not following ignorecase
" files }}}

" text {{{
" searching
set ignorecase                          " case insensitive search by default ...
set smartcase                           " ... unless using uppercase letter
set hlsearch                            " highlight all matches
set incsearch                           " search incrementally

" indentation
set autoindent                          " copy indent form current line
set expandtab                           " insert spaces in place of tabs
set shiftround                          " < and > indent to multiples of sw
set shiftwidth=4                        " indent width
set smarttab                            " sw at the start of a line, sts else
set softtabstop=4                       " amount of columns inserted by <tab>
set tabstop=4                           " visual width of <tab>

set wrap                                " wrap lines too long to display
if has('linebreak')
    set linebreak                       " break lines at sensible places
    set breakindent                     " keep indentation when wrapping
endif

if has('folding')
    set foldenable                      " all folds opened in autocmd
    set foldcolumn=1
    set foldmethod=syntax               " folding based on syntax
endif

set showmatch                           " highlight matching paren-like chars
if !exists('g:loaded_matchit')
    runtime! macros/matchit.vim
endif

set backspace=2                         " backspace over everything in insert

set complete-=i                         " include file completion is slow

set nrformats-=octal                    " predictable number inc/decreasing

if v:version > 703 || v:version == 703 && has('patch541')
    set formatoptions+=j                " remove comment leader on line join
endif

set virtualedit=block                   " free movement in block mode
" text }}}

" windows {{{
if has('mouse')
    set mouse=nvi                       " use mouse in all modes
    if has('mouse_sgr')                 " resize splits with mouse
        set ttymouse=sgr                " work in columns beyond 223
    elseif has('mouse_xterm')
        set ttymouse=xterm2
    endif
endif

set splitbelow                          " put split below current window
set splitright                          " put vertical split to the right

set scrolljump=10                       " jump lines on scrolloff
set scrolloff=1                         " lines around cursor kept on screen
set sidescrolloff=5                     " jump columns on scrolloff
" windows }}}

" ui {{{
set visualbell                          " use visual bell instead of beeping

set number                              " show current line number ...
set relativenumber                      " ... relative for others

" statusline
set cmdheight=2                         " avoid hit-enter prompts
set showcmd                             " show keys in a current chord
set laststatus=2                        " always show statusline
set ruler                               " show cursor position

" dialogues
set confirm                             " show dialog on quitting unsaved
set wildmenu                            " show possible completions
set wildmode=longest:full,full          " complete common, then full matches
" ui }}}

" mappings {{{
set ttimeout                            " timeout on key codes
set ttimeoutlen=100                     " key code timeout

if has('langmap') && exists('+langremap')
    set nolangremap                     " no langmap for results of mappings
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

" :vh to open help in vertical split
cnoreabbrev vh vertical help
" :ho to open help in only window, <S-Left> - backward word in insert mode
cnoreabbrev ho help <Bar> only<S-Left><S-Left><Left>

" if nr of buffers is lower than cmdheight doesn't show anything
" if only one buffer is listed it won't show because cmdheight is set to 2
cnoreabbrev b  ls<CR>:buffer
cnoreabbrev sb ls<CR>:sbuffer
cabbrev     vb ls<CR>:vertical sb

nmap <Leader>b  :b<Space>
nmap <Leader>sb :sb<Space>
nmap <Leader>vb :vb<Space>

func! s:Zoom()
    if exists('w:unzoom')               " w: is window local
        exe w:unzoom
        unl w:unzoom
    el
        let w:unzoom = winrestcmd()
        wincmd |
        wincmd _
    en
endf

" TODO why does @@@ show in other windows, maybe see :h 'display'?
nnoremap <silent> <Leader>z :call <SID>Zoom()<CR>
" mappings }}}

" misc. {{{
if !exists(':DiffOrig')                 " see :h DiffOrig
    command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                \ | wincmd p | diffthis
endif

if has('nvim-0.3.2') || has('patch-8.1.0360')
    set diffopt=internal,filler,algorithm:histogram,indent-heuristic
endif

if &history < 1000
    set history=1000                    " entries in cmd and search histories
endif

if &tabpagemax < 50
    set tabpagemax=50                   " limit for 'vim -p ...' or ':tab all'
endif

set lazyredraw                          " don't redraw while executing macros

set sessionoptions-=options             " don't store options across sessions

set shortmess=aoOtT

let &makeprg = '(make $* \|\| make %<)' " use implicit rule if make fails
" misc. }}}

" netrw {{{
" TODO put 'suffixes' at the end, see vim-vinegar
let g:netrw_sort_sequence   = '[\/]$,*'
let g:netrw_banner          = 0         " disable netrw banner
" TODO bug central
"let g:netrw_liststyle       = 3         " tree view

"let g:netrw_browse_split    = 4         " open in previous window
"let g:netrw_altv            = 0         " controlled by &splitbelow  / splitright?
"let g:netrw_preview         = 1         " preview in vertical split
"let g:netrw_winsize         = 25        " netrw percentage size when previewing
"let g:netrw_cursorline      = 0
" }}}

" nvim terminal {{{
if has('nvim')                          " use <Esc> to exit terminal-mode
    tnoremap <Esc> <C-\><C-n>

    aug terminals
        au!
        " cursor moves weirdly when exiting terminal mode
        " if number / relativenumber is set
        " set syntax to highlight commands
        " start in insert mode
        au TermOpen * setl nonumber norelativenumber syntax=sh  |
                    \ startinsert!
    aug END
endif
" nvim terminall }}}
