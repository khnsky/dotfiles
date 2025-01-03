-- Do as c does.
--
-- Based on $VIMRUNTIME/ftplugin/cpp.vim.
--
-- The [.] in c[.]{vim,lua} is necessary because '{' is not currently treated
-- as wildcard character on Windows.
--
-- See: github.com/neovim/neovim/commit/7b16c1fa8451880c72769f6d3c311f24c74f4fc7
vim.cmd.runtime {
    args = {
        "after/ftplugin/c[.]{vim,lua}",
        "after/ftplugin/c_*.{vim,lua}",
        "after/ftplugin/c/*.{vim,lua}",
    },
    bang = true
}
