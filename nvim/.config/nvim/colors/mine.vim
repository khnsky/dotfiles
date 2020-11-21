set background=dark
hi clear
if exists('syntax_on')
    syntax reset
endif
let g:colors_name = 'mine'

hi CursorLineNr ctermfg=White
hi LineNr       ctermfg=DarkGrey

hi StatusLine   cterm=None          ctermfg=LightGrey
hi StatusLineNC cterm=None          ctermfg=DarkGrey
hi TabLine      cterm=None          ctermfg=DarkGrey    ctermbg=None
hi TabLineFill  cterm=None
hi TabLineSel   cterm=None          ctermfg=White
hi VertSplit    cterm=None          ctermfg=DarkGrey
hi WildMenu     cterm=None          ctermfg=White       ctermbg=None

hi StatusLine   gui=None            guifg=#a0a0a0       guibg=#0f0f0f
hi StatusLineNC gui=None            guifg=#202020       guibg=#0f0f0f
hi TabLine      gui=None            guifg=#808080       guibg=None
hi TabLineFill  gui=None
hi TabLineSel   gui=None            guifg=#dddddd
hi VertSplit    gui=None            guifg=#0f0f0f
hi WildMenu     gui=None            guifg=#dddddd       guibg=None

" :so $VIMRUNTIME/syntax/hitest.vim
    
" :hi clear
" :hi clear {group-name}
" :hi {group-name} NONE

"
" dark, light
" 0, 8  black   #000000 #767676
" 1, 9  red     #cc0403 #f2201f
" 2, 10 green   #19cb00 #23fd00
" 3, 11 yellow  #cecb00 #fffd00
" 4, 12 blue    #0d73cc #1a8fff
" 5, 13 magenta #cb1ed1 #fd28ff
" 6, 14 cyan    #0dcdcd #14ffff
" 7, 15 white   #dddddd #ffffff

" hi! link - the ! overwrites the existing link
hi Comment ctermfg=DarkGrey guifg=#808080
" Constant (String, Character, Number, Boolean, Float)
hi Constant ctermfg=LightCyan guifg=#14ffff
" Identifier (Function)
hi! link Identifier Normal
" Statement (Conditional, Repeat, Label, Operator, Keyword, Exception)
hi Statement gui=None ctermfg=DarkBlue guifg=#0d73cc
" PreProc (Include, Define, Macro, PreCondit)
hi PreProc ctermfg=LightGray guifg=#404040
" Type (StorageClass, Structure, Typedef)
hi! link Type Statement
" Special (SpecialChar, Tag, Delimeter, SpecialComment, Debug)
hi Special ctermfg=LightMagenta guifg=#fd28ff

hi Visual cterm=None ctermbg=Black gui=None guibg=#0f0f0f

"Underlined
"Ignore
"Error
"Todo
"
"ColorColumn
"Conceal
"Cursor
"CursorIM
"CursorColumn
"CursorLine
"Directory
"DiffAdd
"DiffChage
"DiffDelete
"DiffText
"EndOfBuffer
"TermCursor
"TermCursorNC
"ErrorMsg
"VertSplit
"Folded
"FoldColumn
"SignColumn
"IncSearch
"Substitute
"LineNr
"CursorLineNr
"MatchParen
"ModeMsg
"MsgArea
"MsgSeparator
"MoreMsg
"NonText
"Normal
"NormalFloat
"Pmenu
"PmenuSel
"PmenuSbar
"PmenuThumb
"Question
"QuickFixLine
"Search
"SpecialKey
"SpellBad
"SpellCap
"SpellLocal
"SpellRare
"StatusLine
"StatusLineNc
"TabLine
"TabLineFill
"TabLineSel
"Title
"Visual
"VisualNOS
"WarningMsg
"Whitespace
"Wildmenu
"Menu
"Scrollbar
"Tooltip
