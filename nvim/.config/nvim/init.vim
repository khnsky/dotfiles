" vim:fdm=marker:fmr={{{,}}}:fdl=0:fen:ts=4:sts=4:sw=4:et

" init {{{1
" vint: -ProhibitSetNoCompatible
if &compatible                          " avoid side effects if already set
    set nocompatible                    " turn vi compatibility off
endif

silent! while 0
    set nocompatible
silent! endwhile
" vint: +ProhibitSetNoCompatible

if 1                                    " has +eval
    filetype indent plugin on           " enable file type plugins and indent

    augroup vimrc
        autocmd!
    augroup END
endif

if has('syntax')
    if !exists('g:syntax_on')
        syntax enable                   " load syntax highlight once
    endif

    colorscheme mine

    set synmaxcol=300                   " stop searching for syntax items after
endif

" files {{{1
" reload buffer if file detected to have changed
" write file when changing buffers
" disable backup, swap and viminfo files
set autoread autowriteall nobackup noswapfile nowritebackup viminfo=

if has('persistent_undo')               " semi-persistent undo history
    set undofile undodir=/tmp,.         " TODO: not sure about security of this
endif

if has('path_extra')                    " see :h file-searching
    set path=.,,**                      " search down when using 'path' (:find)
    set tags=./tags; tags+=tags         " search for tags in current dir and up in dir of current file
    if has('emacs_tags')
        set tags+=./TAGS; tags+=TAGS    
    endif
endif

set tagcase=match                       " match tags case not following ignorecase

" text {{{1
" case insensitive search by default unless uppercase letter used, highlight
" all matches, serach incrementally
set ignorecase smartcase hlsearch incsearch

if exists('+inccommand')
    set inccommand=split
endif

" use previous line indent
" sane values for c/c++ indentation - see :h cinoptions-values
set autoindent cinoptions=N-s,:0,l1,b0,g0,t0,(0,U1,W1s,m1,j1

" insert spaces in place of tabs, round indent to multiple of 'sw'
" use &tabstop for each step of (auto)indent
" use sw at the start of a line, sts else
" insert 4 spaces for tab and represent tab by 4 columns
set expandtab shiftround shiftwidth=0 smarttab tabstop=4
let &softtabstop = &tabstop

set wrap                                " wrap lines too long to display
if has('linebreak')
    " break lines at sensible places and keep indentation when wrapping
    set linebreak breakindent
endif

if has('folding')
    " enable syntax based folding, folds opened in autocmd
    set nofoldenable foldmethod=syntax
endif

" add <:> and =:; to matching pairs, highligth match to the one under cursor
set matchpairs=(:),{:},[:],<:>,=:; showmatch

" include file completion is slow
" show menu even for single completion, show extra information in preview
" window, force user to select match
set complete-=i completeopt=menuone,preview,noselect

set nrformats=bin,hex                   " predictable number inc/decreasing
set backspace=2                         " backspace over everything in insert
set virtualedit=block                   " free movement in block mode

" formatoptions+=1
if has#('patch-7.3.0541')
    " remove comment leader when joining lines
    set formatoptions+=j
endif

" windows {{{1
" enable mouse support if available
" `nvi` allows to use mouse in most modes but copy text from remote
" vim instance by using command mode
" use ttymouse=xterm2 or sgr when available to resize windows with mouse
silent! set mouse=nvi ttymouse=xterm2 ttymouse=sgr

" put splits below current window and vertical split to the right
set splitbelow splitright

" jump 10 lines and 5 columns on scrolloff, keep 1 line context on screen
set scrolljump=10 scrolloff=1 sidescrolloff=5

" ui {{{1
set visualbell                          " use visual bell instead of beeping

" avoid hit-enter prompts, show keys in a current chord, always show
" statusline, show cursor position in statusline
set cmdheight=2 showcmd laststatus=2 ruler

" show dialogs on eg. quitting unsaved, show possible completiions
" complete common then full matches
" ignore implied directories, ignore case in file completion
set confirm wildmenu wildmode=longest:full,full wildignore=./,../ wildignorecase

" all default but use line cursor for command mode
set guicursor=n-v-sm:block,i-c-ci-ve:ver25,r-cr-o:hor20

" use line cursor for insert mode, block cursor for normal in vim
" might not work for one terminal or another
if !has('nvim')
    let &t_SI = "\e[6 q"
    let &t_EI = "\e[2 q"

    autocmd vimrc VimEnter * silent! !print -n "\e[2 q"
endif

" mappings {{{1
" short timeout on key codes
set ttimeout ttimeoutlen=100

if has('langmap') && exists('+langremap')
    " no langmap for results of mappings
    set nolangremap
endif

" break undo on <C-U>
inoremap <C-U> <C-G>u<C-U>

" break undo on newline
if has#('patch-7.3.0489')
    " expand abbreviations
    inoremap <CR> <C-G>u<C-]><CR>
else
    " without this patch it inserts literal ^]
    inoremap <CR> <C-G>u<CR>
endif

let mapleader = '\'
map <Space> <Leader>

" use Q for formatting, there is gQ for Ex
map Q gq
" copy to EOL instead of copying line, more analogous to the rest of operators
map Y y$

" try to keep cursor in the same column when scrolling, moving, jumping
" go to the column  of the mark instead of going to the first non-blank
set nostartofline
map ' `

" clear search highlight in addition to clearing & redrawing screen
if has('nvim')
    nnoremap <C-L> <Cmd>nohl<CR><C-L>
else
    nnoremap <C-L> :nohl<CR><C-L>
endif

" don't expand abbreviations in console with Return
" mapped Return doesn't trigger expansion even if it is mapped to itself
cnoremap <CR> <CR>

" :vh to open help in vertical split
cnoreabbrev vh vertical help
" :ho to open help in only window, <S-Left> - backward word in insert mode
cnoreabbrev ho help <Bar> only<S-Left><S-Left><Left>

nmap <silent> <Leader>z <Plug>ZoomIt

xnoremap < <gv
xnoremap > >gv

map <Leader>h <C-w>h
map <Leader>j <C-w>j
map <Leader>k <C-w>k
map <Leader>l <C-w>l

map <Leader>H <C-w>H
map <Leader>J <C-w>J
map <Leader>K <C-w>K
map <Leader>L <C-w>L

nnoremap <tab>   <c-w>w
nnoremap <s-tab> <c-w>W

nnoremap [q :cprev<cr>zz
nnoremap ]q :cnext<cr>zz
nnoremap [l :lprev<cr>zz
nnoremap ]l :lnext<cr>zz

nnoremap <silent> <leader>c :cclose <bar> lclose<cr>

nnoremap [b :bprev<cr>
nnoremap ]b :bnext<cr>

nnoremap [t :tabp<cr>
nnoremap ]t :tabn<cr>

nnoremap <leader>o o<c-[>
nnoremap <leader>O O<c-[>

" prevent code execution when pasting from system clipboard
" <c-r><c-r> inserts text from register like <c-r> but does so literally
inoremap <c-r>+ <c-r><c-r>+
inoremap <c-r>* <c-r><c-r>*

" <c-w>] and <c-w><c-]> do the same - make so <c-w><c-]> opens tag in vsplit
nnoremap <c-w><c-]>  :exe "vert stag" expand('<cword>')<cr>

" misc. {{{1
if has('nvim-0.3.2') || has#('patch-8.1.0360')
    " use internal diff with better diffing algorithm
    set diffopt=internal,filler,algorithm:histogram,indent-heuristic
endif

" vint: -ProhibitAbbreviationOption
let &history    = max([&hi,  1000])     " entries in cmd and search histories
let &tabpagemax = max([&tpm, 50])       " limit for 'vim -p ...' or ':tab all'
" vint: +ProhibitAbbreviationOption

set lazyredraw                          " don't redraw while executing macros
set sessionoptions-=options             " don't store options across sessions
set shortmess=aoOtT
