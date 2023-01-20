-- |¯¯¯|/¯¯¯/ |¯¯¯\|¯¯¯|  /¯¯¯¯¯\  |¯¯¯¯¯¯|  |
-- |       <  |        | |   x   | |_    _|  |
-- |___|\___\ |___|\___|  \_____/    |__|    |
-- ------------------------------------------|
-- Personal configuration file for neovim    |
-- __________________________________________|

-- TODO: Treesitter text objects
-- TODO: Fix autocomplete

require("fuglesteg.plugins")
require("fuglesteg.keymaps")
require("fuglesteg.plugin-config")
require("fuglesteg.neovide")
require("fuglesteg.terminal")

-- Basic settings
-- set number
vim.opt.number = true
vim.opt.syntax = "on"
vim.opt.wildmenu = true
vim.opt.cursorline = true

vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.termguicolors = true
vim.cmd('colorscheme tokyonight')

vim.opt.scrolloff = 8

vim.wo.colorcolumn = '80'

vim.o.termguicolors = true
-- Dirty yucky mouse support
vim.opt.mouse = 'a'

