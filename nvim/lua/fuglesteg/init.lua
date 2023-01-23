-- |¯¯¯|/¯¯¯/ |¯¯¯\|¯¯¯|  /¯¯¯¯¯\  |¯¯¯¯¯¯|  |
-- |       <  |        | |   x   | |_    _|  |
-- |___|\___\ |___|\___|  \_____/    |__|    |
-- ------------------------------------------|
-- Personal configuration file for neovim    |
-- __________________________________________|

-- TODO: Treesitter text objects
-- TODO: Add indent guide
-- TODO: Add git gutter
-- TODO: Add autopairs (https://github.com/hrsh7th/nvim-cmp/wiki/Advanced-techniques#add-parentheses-after-selecting-function-or-method-item)
-- FIXME: Neogit crashes after sending second commit, empty confirm message

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

-- set tab
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true


vim.cmd('colorscheme tokyonight')

vim.o.wrap = false
vim.opt.scrolloff = 8
vim.wo.colorcolumn = '80'

vim.o.termguicolors = true

vim.opt.mouse = 'a'

-- Alacritty
vim.cmd([[
if $TERM == 'alacritty'
      set ttymouse=sgr
endif
]])
