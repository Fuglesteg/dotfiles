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

-- Dirty yucky mouse support
vim.opt.mouse = 'a'

vim.o.termguicolors = true

-- Neovide
vim.opt.guifont = "mononoki Nerd Font Mono:h14"
vim.g.neovide_cursor_trail_length = 0.01
vim.g.neovide_cursor_animation_length = 0.01
vim.g.neovide_fullscreen = true
vim.g.neovide_refresh_rate = 144
vim.g.neovide_refresh_rate_idle = 5
vim.g.neovide_scroll_animation_length = 0.3

-- Terminal autocmd
local disableNumbers = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
end

vim.api.nvim_create_autocmd({"TermOpen"}, {
    pattern = {"term://*"},
    callback = disableNumbers,
})
