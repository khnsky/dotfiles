-- write file when changing buffers
vim.o.autowriteall = true

vim.o.undofile = true

-- search down when using 'path' (:find)
vim.o.path = '.,,**'

vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.autoindent = true

-- see :h 'tabstop'
-- see :h ins-expandtab
vim.o.expandtab = true              -- insert spaces in place of tabs
vim.o.shiftround = true             -- round indent to multiple of 'shiftwidth'
vim.o.shiftwidth = 0                -- use value of tabstop
vim.o.tabstop = 4                   -- number of spacces tab counts for
vim.o.softtabstop = vim.o.tabstop

-- break lines at sensible places and keep indentation when wrapping
vim.o.linebreak = true
vim.o.breakindent = true

-- enable syntax based folding, folds opened in autocmd
vim.o.foldenable = false
vim.o.foldmethod = "syntax"

-- add <:> and =:; to matching pairs, highlight match to the one under cursor
vim.opt.matchpairs:append("<:>,=:;")
vim.o.showmatch = true

-- show menu even for single completion, show extra information in preview
-- window, force user to select match
vim.o.completeopt = "menuone,preview,noselect"

-- free movement in block mode
vim.o.virtualedit = "block"

-- see :h fo-table
-- 1 - don't break lines after one-letter words, break it before if possible
-- l - don't break lines that were too long before entering insert
-- n - recognize numbered lists (using 'formatlistpat') and indent accordingly
-- p - don't break lines at single spaces following periods
--
-- t and c flags (auto-wrap of text and comments respectively) should be set in
-- appropriate ftplugins
vim.opt.formatoptions:append("1lnp")

-- see :h cpo-J
-- joinspaces inserts two spaces after '.', '?' or '!' with join command
-- a sentence has to be followed by two spaces after '.', '?' or '!'
vim.o.joinspaces = true
vim.opt.cpoptions:append("J")

-- toggle and print spell with <leader>s
vim.o.spelllang = "pl,en_us,en_gb"
vim.keymap.set('n', '<leader>s', function()
    vim.o.spell = not vim.o.spell
    print(vim.o.spell and "spell" or "nospell") -- ??
end)
-- fix last spelling mistake by selecting first suggestion
vim.keymap.set('i', '<C-l>', '<c-g>u<Esc>[s1z=`]a<c-g>u')

-- because of using two-spaced sentences spellcapcheck can be more restrictive
vim.o.spellcapcheck = [[[.?!]\%(  \|[\n\r\t]\)]]
vim.opt.spelloptions:append("camel")

-- put splits below current window and vertical split to the right
vim.o.splitbelow = true
vim.o.splitright = true

-- jump 10 lines and 5 columns on scrolloff, keep 1 line context on screen
vim.o.scrolljump = 10
vim.o.scrolloff = 1
vim.o.sidescrolloff = 5

-- use visual bell instead of beeping
vim.o.visualbell = true

vim.o.cmdheight = 0
vim.o.laststatus = 3
vim.o.showcmdloc = "statusline"
-- see :h 'statusline'
-- vim.o.winbar = [[%<%f\ %h%m%r%=%-14.(%l,%c%V%)\ %P]]
vim.o.winbar = "%<%t"

-- show dialogues on eg. quitting unsaved, show possible completions
-- complete common then full matches
-- ignore implied directories, ignore case in file completion
vim.o.confirm = true
vim.o.wildmode = "longest:full,full"
vim.o.wildignorecase = true
vim.o.wildignore = "./,../,**/./,**/../"

-- all default but use line cursor for command mode
vim.o.guicursor = "n-v-sm:block,i-c-ci-ve:ver25,r-cr-o:hor20"

-- break undo on newline, expand abbreviations
-- see examples in :h ins-special-special
vim.keymap.set('i', "<CR>", "<C-G>u<C-]><CR>")

vim.keymap.set('n', '<Space>', '<Leader>', { remap = true })
vim.keymap.set('n', 'Q', 'gq')
vim.keymap.set('n', "'", '`')

vim.keymap.set('n', '<Leader>z', '<Plug>ZoomIt')

vim.keymap.set('x', '<', '<gv')
vim.keymap.set('x', '>', '>gv')

vim.keymap.set('n', '<tab>',   '<c-w>w')
vim.keymap.set('n', '<s-tab>', '<c-w>W')

vim.keymap.set('n', '[q', '<cmd>cprev<cr>zz')
vim.keymap.set('n', ']q', '<cmd>cnext<cr>zz')
vim.keymap.set('n', '[l', '<cmd>lprev<cr>zz')
vim.keymap.set('n', ']l', '<cmd>lnext<cr>zz')
vim.keymap.set('n', '[b', '<cmd>bprev<cr>')
vim.keymap.set('n', ']b', '<cmd>bnext<cr>')
vim.keymap.set('n', '[t', '<cmd>tabp<cr>')
vim.keymap.set('n', ']t', '<cmd>tabn<cr>')

vim.keymap.set('n', '<leader>c', function()
    vim.cmd.cclose()
    vim.cmd.lclose()
end)

vim.keymap.set('n', '<leader>o', 'o<c-[>')
vim.keymap.set('n', '<leader>O', 'O<c-[>')

-- prevent code execution when pasting from system clipboard
-- <c-r><c-r> inserts text from register like <c-r> but does so literally
vim.keymap.set('i', '<c-r>+', '<c-r><c-r>+')
vim.keymap.set('i', '<c-r>*', '<c-r><c-r>*')

-- https://vim.fandom.com/wiki/Moving_lines_up_or_down
vim.keymap.set('i', '<a-j>', '<esc>:m .+1 <cr>gi')
vim.keymap.set('n', '<a-j>',      ':m .+1 <cr>')
vim.keymap.set('v', '<a-j>',      ":m '>+1<cr>gv")
vim.keymap.set('i', '<a-k>', '<esc>:m .-2 <cr>gi')
vim.keymap.set('n', '<a-k>',      ':m .-2 <cr>')
vim.keymap.set('v', '<a-k>',      ":m '<-2<cr>gv")

-- paste over selection without overwriting register
vim.keymap.set('v', 'p', '"_dP')

vim.o.diffopt = "internal,filler,algorithm:histogram,indent-heuristic"
vim.o.smoothscroll = true

-- TODO
vim.opt.wildoptions:append("fuzzy")

local packages = require "packages"
local github   = packages.sources.github

packages {
    github "hrsh7th/nvim-cmp",

    github "hrsh7th/cmp-buffer",
    github "hrsh7th/cmp-cmdline",
    github "hrsh7th/cmp-nvim-lsp",
    github "hrsh7th/cmp-path",

    github "neovim/nvim-lspconfig",
}

local cmp = require "cmp"

cmp.setup {
    mapping = cmp.mapping.preset.insert {
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs( 4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<CR>'] = cmp.mapping.confirm { select = false },
    },

    sources = cmp.config.sources({
        { name = "nvim_lsp" },
        -- TODO: add vim.snippet as source
        -- reddit.com/r/neovim/comments/1cxfhom/builtin_snippets_so_good_i_removed_luasnip/
    }, {
        { name = "buffer"   },
    }),
}

cmp.setup.cmdline({ "/", "?" }, {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = "buffer" },
    },
})

cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    }),
    matching = { disallow_symbol_nonprefix_matching = false }
})

local lspconfig = require "lspconfig"

local function on_attach(client, bufnr)
    vim.bo.omnifunc = "v:lua.vim.lsp.omnifunc"

    local fntocmd = require "util".fntocmd

    local function map(maps)
        local opts = {
            noremap = true,
            silent  = true,
        }
        for k, v in pairs(maps) do
            local cmd = "<cmd>" .. fntocmd(v) .. "<cr>"
            vim.api.nvim_buf_set_keymap(bufnr, "n", k, cmd, opts)
        end
    end

    local diagnostic = vim.diagnostic or vim.lsp.diagnostic
    map {
        ["gD"]          = vim.lsp.buf.declaration,
        ["gd"]          = vim.lsp.buf.definition,
        ["K"]           = vim.lsp.buf.hover,
        ["gi"]          = vim.lsp.buf.implementation,
        ["<c-k>"]       = vim.lsp.buf.signature_help,
        ["<space>D"]    = vim.lsp.buf.type_definition,
        ["<space>rn"]   = vim.lsp.buf.rename,
        ["<space>ca"]   = vim.lsp.buf.code_action,
        ["gr"]          = vim.lsp.buf.references,
        ["<space>e"]    = diagnostic.goto_prev,
        ["[d"]          = diagnostic.goto_prev,
        ["]d"]          = diagnostic.goto_next,
        ["<space>q"]    = diagnostic.set_loclist,
        ["<space>f"]    = vim.lsp.buf.formatting,
    }
end

for _, server in ipairs { "clangd" } do
    lspconfig[server].setup {
        capabilities = require 'cmp_nvim_lsp'.default_capabilities(),
        on_attach    = on_attach,
        flags        = {
            debounce_text_changes = 150,
        }
    }
end

