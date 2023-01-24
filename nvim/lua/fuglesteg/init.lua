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

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Indenting
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.autoindent = true
vim.opt.smarttab = true

-- Visual
vim.opt.termguicolors = true
vim.cmd('colorscheme tokyonight')

vim.o.wrap = false
vim.opt.scrolloff = 8
vim.wo.colorcolumn = '80'
vim.opt.cursorline = true
vim.opt.wildmenu = true
vim.opt.linebreak = true
vim.opt.wrap = false

-- Dirty yucky mouse support
vim.opt.mouse = 'a'

-- Alacritty
vim.cmd([[
if $TERM == 'alacritty'
      set ttymouse=sgr
endif
]])
