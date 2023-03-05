-- |¯¯¯|/¯¯¯/ |¯¯¯\|¯¯¯|  /¯¯¯¯¯\  |¯¯¯¯¯¯|  |
-- |       <  |        | |   x   | |_    _|  |
-- |___|\___\ |___|\___|  \_____/    |__|    |
-- ------------------------------------------|
-- Personal configuration file for neovim    |
-- __________________________________________|

-- TODO: Treesitter text objects
-- TODO: Look into navigate.nvim
-- TODO: Fix java
-- FIXME: Neogit crashes after sending second commit, empty confirm message
-- TODO: This was caused by Noice.nvim, check if it is fixed in Neovim nightly, as it was described in a reddit comment by Folke
-- TODO: Change z hydra to use same command as scrolling horizontally (Maybe nvim_input_mouse ??)

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
require("fuglesteg.lazy")
require("fuglesteg.keymaps")
require("fuglesteg.neovide")
require("fuglesteg.terminal")

-- Basic settings
-- set number
vim.opt.number = true
vim.opt.syntax = "on"

-- List highlight
vim.opt.list = true
-- 
vim.opt.listchars = [[tab:» ,trail:·,nbsp:,precedes:<,extends:>]]
-- Highlight nbsp as error
vim.cmd([[match Error /\%xA0/]])

-- Highlight in command line window
vim.api.nvim_create_autocmd({"CmdwinEnter"}, {
  callback = function()
    vim.api.nvim_buf_set_option(0, "syntax", "vim")
  end
})

-- Set diff view to always start as vertical, other options are default:
-- Default options: internal,filler,closeoff
vim.opt.diffopt = "internal,filler,closeoff,vertical"

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Indenting
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true

-- Visual
vim.opt.termguicolors = true
vim.cmd('colorscheme tokyonight')

vim.opt.signcolumn = 'yes'
vim.o.wrap = false
vim.opt.scrolloff = 8
vim.wo.colorcolumn = '80'
vim.opt.cursorline = true
vim.opt.wildmenu = true
vim.opt.linebreak = true
vim.opt.wrap = false

-- Dirty yucky mouse support
vim.opt.mouse = 'a'
