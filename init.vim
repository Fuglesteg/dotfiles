"" |¯¯¯|/¯¯¯/ |¯¯¯\|¯¯¯|  /¯¯¯¯¯\  |¯¯¯¯¯¯|  |
"" |       <  |        | |   x   | |_    _|  |
"" |___|\___\ |___|\___|  \_____/    |__|    |
"" ------------------------------------------|
"" Personal configuration file for neovim    |
"" __________________________________________|

" Basic settings

set number
syntax on
set wildmenu



" Plugins

call plug#begin()

Plug 'tpope/vim-surround'

Plug 'tpope/vim-commentary'

Plug 'folke/which-key.nvim'

Plug 'romgrk/doom-one.vim'

Plug 'folke/tokyonight.nvim', { 'branch': 'main' }

Plug 'nvim-lualine/lualine.nvim'

Plug 'kyazdani42/nvim-web-devicons'

Plug 'nvim-lua/plenary.nvim'

Plug 'nvim-telescope/telescope.nvim'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'neovim/nvim-lspconfig'

call plug#end()

let g:doom_one_terminal_colors = v:true

colorscheme tokyonight

lua << EOF
	require('lualine').setup{
		options = { theme = 'tokyonight' }
	}
EOF

" Keybindings

let g:mapleader = "\<Space>"
let g:maplocalleader = ","

" Which-key config
lua << EOF
	local wk = require("which-key")
	wk.register({
		f = {
			name = "+File",
			w = { ":w<cr>", "Save file" },
			W = { ":wall<cr>", "Save all files" },
			q = { ":wq<cr>", "Write and quit file" },
			Q = { ":wqall<cr>", "Write and quit all files" },
			-- ! = { ":qall!<cr>", "Quit all files" }
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
	}, { prefix = "<leader>" })
EOF
