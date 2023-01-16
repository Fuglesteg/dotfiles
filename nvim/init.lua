-- |¯¯¯|/¯¯¯/ |¯¯¯\|¯¯¯|  /¯¯¯¯¯\  |¯¯¯¯¯¯|  |
-- |       <  |        | |   x   | |_    _|  |
-- |___|\___\ |___|\___|  \_____/    |__|    |
-- ------------------------------------------|
-- Personal configuration file for neovim    |
-- __________________________________________|

-- TODO: Add magit (https://github.com/TimUntersberger/neogit)
-- TODO: Add git diff

-- Basic settings
-- set number
vim.opt.number = true
vim.opt.syntax = "on"
vim.opt.wildmenu = true

vim.opt.softtabstop = 4
vim.opt.expandtab = true

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

-- Plugins

-- Install packer
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.system { 'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path }
  vim.cmd [[packadd packer.nvim]]
end

require('packer').startup(function(use)
  -- Package manager
  use 'wbthomason/packer.nvim'
  use {
      'VonHeikemen/lsp-zero.nvim',
      requires = {
        -- LSP Support
        {'neovim/nvim-lspconfig'},
        {'williamboman/mason.nvim'},
        {'williamboman/mason-lspconfig.nvim'},

        -- Autocompletion
        {'hrsh7th/nvim-cmp'},
        {'hrsh7th/cmp-buffer'},
        {'hrsh7th/cmp-path'},
        {'saadparwaiz1/cmp_luasnip'},
        {'hrsh7th/cmp-nvim-lsp'},
        {'hrsh7th/cmp-nvim-lua'},

        -- Snippets
        {'L3MON4D3/LuaSnip'},
        -- Snippet Collection (Optional)
        {'rafamadriz/friendly-snippets'},
        }
    }
    use 'amadeus/vim-convert-color-to'
    use 'tpope/vim-surround'

    use 'tpope/vim-commentary'

    use 'folke/which-key.nvim'

-- Colorschemes
    use 'romgrk/doom-one.vim'
    use 'folke/tokyonight.nvim'

    use 'nvim-lualine/lualine.nvim'

    use 'kyazdani42/nvim-web-devicons'

    use 'nvim-lua/plenary.nvim'

    use 'nvim-telescope/telescope.nvim'

    use 'nvim-treesitter/nvim-treesitter'

    use 'voldikss/vim-floaterm'

    use 'ahmedkhalf/project.nvim'

    use 'kyazdani42/nvim-tree.lua'

    use 'folke/trouble.nvim'

    use 'folke/todo-comments.nvim'
end)

vim.cmd('colorscheme tokyonight')

-- Plugin config

-- LSP config
local lsp = require('lsp-zero')
lsp.preset('recommended')

lsp.setup()

-- Treesitter
require("nvim-treesitter.configs").setup {
    highlight = {
        enable = true
    }
}

-- Lua line config
local function statusClock()
    local timeTable = date()
    local timeHourMinute = timeTable.hour
    return timeHourMinute
end

require('todo-comments').setup { }
require('trouble').setup { }
require('lualine').setup {
    options = { theme = 'tokyonight' },
    sections = { lualine_a = { statusClock } }
}

-- Config for project.nvim and nvim-tree
-- Vim Script
vim.api.nvim_set_var('nvim_tree_respect_buf_cwd', 1)

require("project_nvim").setup {
		manual_mode = false,
		detection_methods = { "lsp", "pattern" },
		patterns = { ".git", "package.json" },
		--silent_chdir = false
}
local telescope = require("telescope")
telescope.load_extension("projects")
telescope.setup {
	defaults = {
		file_ignore_patterns = {
			"%.png",
			"%.jpg",
			"%.jpeg",
			"%.gif"
		}
	}
}

require("nvim-tree").setup({
  update_cwd = true,
  update_focused_file = {
    enable = true,
    update_cwd = true,
  },
actions= {
    change_dir = {
        global = true
        }
    }
})

vim.diagnostic.config({
    virtual_text = true,
    signs = true,
})
vim.opt.signcolumn = 'yes'

-- Keybindings

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Utility functions

local function remap(mode, binding, command)
    vim.keymap.set(mode, binding, command, { silent = true })
end

local function nmap(binding, command)
    remap('n', binding, command)
end

local function vmap(binding, command)
    remap('v', binding, command)
end

local function imap(binding, command)
    remap('i', binding, command)
end

local function tmap(binding, command)
    remap('t', binding, command)
end

-- nmap <silent><Esc> :nohl<cr>
nmap("<Esc>", ":FloatermHide<cr>:nohl<cr>")
nmap("<leader><leader>", ":Telescope find_files<cr>")
tmap("<C-a>", "<C-\\><C-n>")
-- inoremap <C-j> <Down>
-- inoremap <C-k> <Up>
-- inoremap <C-h> <Left>
-- inoremap <C-l> <Right>
nmap("<C-j>", "<Tab>")
nmap("<C-k>", "<S-Tab>")
nmap("<leader>;", ":Commentary<cr>")
vmap("<leader>;", ":Commentary<cr>")
vmap(">", ">gv")
vmap("<", "<gv")
nmap("<silent>J", "gT")
nmap("<silent>K", "gt")
-- nnoremap <C-v> +p
nmap("<silent>gd", ":lua vim.lsp.buf.definition()<cr>")
nmap("<silent>gr", ":Telescope lsp_references<cr>")

imap("<c-space>", "lua vim.lsp.buf.definition()<cr>")

-- Which-key config
local wk = require("which-key")
wk.register({
    f = {
        name = "+File",
        s = { ":w<cr>", "Save file" },
        S = { ":wall<cr>", "Save all files" },
        q = { ":wq<cr>", "Write and quit file" },
        Q = { ":wqall<cr>", "Write and quit all files" },
        f = { ":NvimTreeOpen<cr>", "Open NvimTree" },
        r = { ":Telescope oldfiles<cr>", "Open recent files" },
        --! = { ":qall!<cr>", "Quit all files" },
    },
    w = {
        name = "+Window",
        v = { ":vs<cr>", "Vertical Split" },
        s = { ":split<cr>", "Horizontal Split" },
        l = { "<C-w>l", "Go left" },
        h = { "<C-w>h", "Go right" },
        j = { "<C-w>j", "Go down" },
        k = { "<C-w>k", "Go up" },
        d = { "<C-w>c", "Close" },
    },
    b = {
        name = "+Buffer",
        b = { ":Telescope buffers<cr>", "Buffers" }
    },
    c = {
        name = "+Code",
        s = { ":Telescope live_grep<cr>", "Fuzzy find" },
        a = { ":lua vim.lsp.buf.code_action()<cr>", "Actions" },
        e = { ":lua vim.lsp.buf.rename()<cr>", "Rename" },
        d = { ":lua vim.lsp.buf.definition()<cr>", "Go to definition" },
        r = { ":Telescope lsp_references<cr>", "Go to references" },
        f = { ":Telescope lsp_document_symbols", "Find symbols in file" },
    },
    C = { ":e ~/.config/nvim/init.lua<cr>", "Open config file" },
    t = {
        name = "+Terminal",
        f = { ":FloatermToggle<cr>", "Toggle terminal" },
        h = { ":FloatermNext<cr>", "Next terminal" },
        l = { ":FloatermPrev<cr>", "Previous terminal" },
        d = { ":FloatermKill<cr>", "Kill terminal" },
        n = { ":FloatermNew<cr>", "New terminal" },
        t = { ":term<cr>", "Open new terminal in buffer" },
    },
    p = {
        name = "+Projects",
        p = { ":Telescope projects<cr>", "Recent projects" }
    },
    h = { ":lua vim.lsp.buf.hover()<cr>", "View documentation" },
}, { prefix = "<leader>" })

