-- github.com/neovim/neovim/commit/054a287dbe6fe4308d26ec593da2057641f2bb9b
vim.bo.commentstring = '// %s'

vim.g.c_comment_strings = 1
vim.g.c_gnu             = 1
vim.g.c_no_ansi         = 1
vim.g.c_no_bsd          = 1
vim.g.c_space_errors    = 1

vim.b.undo_ftplugin = vim.b.undo_ftplugin .. [[
unlet! g:c_gnu
unlet! g:c_space_errors
unlet! g:c_no_ansi
unlet! g:c_no_bsd
unlet! g:c_comment_strings
unlet! g:c_space_errors
]]
