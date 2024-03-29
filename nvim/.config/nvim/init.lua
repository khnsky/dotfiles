vim.cmd "runtime vimrc"

local packages = require "packages"
local github   = packages.sources.github

packages {
    github "hrsh7th/nvim-cmp",

    github "hrsh7th/cmp-buffer",
    github "hrsh7th/cmp-cmdline",
    github "hrsh7th/cmp-nvim-lsp",
    github "hrsh7th/cmp-path",

    github "hrsh7th/vim-vsnip",
    github "hrsh7th/cmp-vsnip",

    github "neovim/nvim-lspconfig",
}

local cmp = require "cmp"

cmp.setup {
    mappings = {
        ["<c-b>"]     = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
        ["<c-f>"]     = cmp.mapping(cmp.mapping.scroll_docs( 4), { "i", "c" }),
        ["<c-space>"] = cmp.mapping(cmp.mapping.complete(),      { "i", "c" }),
        ["<c-y>"]     = cmp.mapping {
            i = cmp.mapping.abort(),
            c = cmp.mapping.close(),
        },
        ["<cr>"]      = cmp.mapping.confirm { select = true },
    },

    snippet = {
        expand = function(args)
            vim.fn["vsnip#anonymous"](args.body)
        end,
    },

    sources = cmp.config.sources {
        { name = "nvim_lsp" },
        { name = "vsnip"    },
        { name = "buffer"   },
    },
}

cmp.setup.cmdline("/", {
    sources = {
        { name = "buffer" },
    },
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

