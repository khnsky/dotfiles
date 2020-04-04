hi clear
if exists('syntax_on')
    syntax reset
endif

set background=dark
let g:colors_name = 'khnsky'

hi Comment ctermfg=Grey
hi Constant ctermfg=Yellow
" jak to
hi! Identifier link Normal
hi Statement ctermfg=Blue
hi PreProc ctermfg=Cyan
hi! link Statement Type
hi Special ctermfg=Magenta


" hi! overwrites highlight links
" h: groups-name
" dark grey
Comment
" light grey
Constant (String, Character, Number, Boolean, Float)
" Normal
Identifier (Function)
" blue
Statement (Conditional, Repeat, Label, Operator, Keyword, Exception)
" dark or light grey
PreProc (Include, Define, Macro, PreCondit)
" blue
Type (StorageClass, Structure, Typedef)
" magenta
Special (SpecialChar, Tag, Delimiter, SpecialComment, Debug)

Underlined
Ignore
Error
Todo

" h: highlight-groups
ColorColumn
Conceal
Cursor
CursorIM
CursorColumn
CursorLine
Directory
DiffAdd
DiffChage
DiffDelete
DiffText
EndOfBuffer
TermCursor
TermCursorNC
ErrorMsg
VertSplit
Folded
FoldColumn
SignColumn
IncSearch
Substitute
LineNr
CursorLineNr
MatchParen
ModeMsg
MsgArea
MsgSeparator
MoreMsg
NonText
Normal
NormalFloat
Pmenu
PmenuSel
PmenuSbar
PmenuThumb
Question
QuickFixLine
Search
SpecialKey
SpellBad
SpellCap
SpellLocal
SpellRare
StatusLine
StatusLineNc
TabLine
TabLineFill
TabLineSel
Title
Visual
VisualNOS
WarningMsg
Whitespace
Wildmenu
Menu
Scrollbar
Tooltip

lua << EOF
local function colors(c)
    return setmetatable(c, {
        _index = function(t, k)
            return rawget(t, k) or 'NONE'
        end,
    })
end
EOF
