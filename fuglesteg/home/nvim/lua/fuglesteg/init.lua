-- |¯¯¯|/¯¯¯/ |¯¯¯\|¯¯¯|  /¯¯¯¯¯\  |¯¯¯¯¯¯|  |
-- |       <  |        | |   x   | |_    _|  |
-- |___|\___\ |___|\___|  \_____/    |__|    |
-- ------------------------------------------|
-- Personal configuration file for neovim    |
-- __________________________________________|

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '
vim.opt.updatetime = 300
vim.opt.conceallevel = 2

-- Basic settings
-- set number
vim.opt.title = true
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
vim.opt.showtabline = 2
vim.opt.laststatus = 3
vim.o.termguicolors = true

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

require("fuglesteg.lazy")
require("fuglesteg.keymaps")
require("fuglesteg.neovide")
require("fuglesteg.terminal")
vim.cmd('colorscheme tokyonight')
