set nocompatible	" be iMproved

filetype off		" required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" The following are examples of different formats supported.
" Keep Plugin commands between vundle#begin/end.
" plugin on GitHub repo
Plugin 'tpope/vim-fugitive'

Plugin 'scrooloose/nerdtree'

Plugin 'kien/ctrlp.vim'

Plugin 'vim-syntastic/syntastic'

" All of your Plugins must be added before the following line
call vundle#end()            " required

" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this lineype detection, loading filetype plugins and filetype indent plugins
filetype indent plugin on
"enable syntax
syntax on
"enable line numbering and relative line numbering
set number relativenumber numberwidth=4
"makes commands show in right down corner
set showcmd

"show end of line and tab chars (invisibles)
set list listchars=tab:▸\ ,eol:¬
"set tabs to 4 spaces length and use tabs not spaces
set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab

"
set hidden
"
set wildmenu
"
"
set ignorecase smartcase
"
set wildmenu
"
"search case insenitive unless cap. letter used or \C flag
set ignorecase smartcase
"display confiramtion prompt instead of error?
set confirm
