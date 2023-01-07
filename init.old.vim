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

" Tabs are 4 spaces, convert tabs to spaces
set tabstop=4 shiftwidth=4 expandtab

" Dirty yucky mouse support
set mouse=a

" Paste from system register
set clipboard^=unnamed,unnamedplus

" Make the default vim find command search recursively
set path+=**

" Neovide
set guifont=mononoki\ Nerd\ Font\ Mono:h12
let g:neovide_cursor_trail_length=0.01
let g:neovide_cursor_animation_length=0.01
let g:neovide_fullscreen=v:true
let g:neovide_refresh_rate=144

" Plugins

call plug#begin()

Plug 'amadeus/vim-convert-color-to'

Plug 'tpope/vim-surround'

Plug 'tpope/vim-commentary'

Plug 'folke/which-key.nvim'

" Colorschemes
Plug 'romgrk/doom-one.vim'
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }

Plug 'nvim-lualine/lualine.nvim'

Plug 'kyazdani42/nvim-web-devicons'

Plug 'nvim-lua/plenary.nvim'

Plug 'nvim-telescope/telescope.nvim'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'voldikss/vim-floaterm'

Plug 'ahmedkhalf/project.nvim'

Plug 'kyazdani42/nvim-tree.lua'

Plug 'folke/trouble.nvim'

Plug 'folke/todo-comments.nvim'

runtime lspPlugins.vim

call plug#end()

colorscheme tokyonight

" Plugin config

" LSP config
runtime lsp.vim

" Lua line config
lua << EOF

	function statusClock()
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

	
EOF
" Config for project.nvim and nvim-tree
" Vim Script
let g:nvim_tree_respect_buf_cwd = 1

lua << EOF
	require("project_nvim").setup {
		manual_mode = false,
		detection_methods = { "lsp", "pattern" },
		patterns = { ".git", "package.json" },
		--silent_chdir = false
	}
	require("telescope").load_extension("projects")
	require("telescope").setup {
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
EOF

" Keybindings

let g:mapleader = "\<Space>"
let g:maplocalleader = ","

" nmap <silent><Esc> :nohl<cr>
nmap <silent><Esc> :FloatermHide<cr>:nohl<cr>
nnoremap <silent><leader><leader> :Telescope find_files<cr>
tnoremap <Esc> <C-\><C-n>
" inoremap <C-j> <Down>
" inoremap <C-k> <Up>
" inoremap <C-h> <Left>
" inoremap <C-l> <Right>
nnoremap <C-j> <Tab>
nnoremap <C-k> <S-Tab>
nnoremap <leader>; :Commentary<cr>
vnoremap <leader>; :Commentary<cr>
vnoremap > >gv
vnoremap < <gv
nnoremap <silent>J gT
nnoremap <silent>K gt
" nnoremap <C-v> +p
nnoremap <silent>gd :lua vim.lsp.buf.definition()<cr>
nnoremap <silent>gr :Telescope lsp_references<cr>

" Which-key config
lua << EOF
	local wk = require("which-key")
	wk.register({
		f = {
			name = "+File",
			s = { ":w<cr>", "Save file" },
			S = { ":wall<cr>", "Save all files" },
			q = { ":wq<cr>", "Write and quit file" },
			Q = { ":wqall<cr>", "Write and quit all files" },
			f = { ":Texplore<cr>", "Open NvimTree" },
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
		t = {
			name = "+Terminal",
			t = { ":FloatermToggle<cr>", "Toggle terminal" },
			K = { ":FloatermNext<cr>", "Next terminal" },
			J = { ":FloatermPrev<cr>", "Previous terminal" },
			d = { ":FloatermKill<cr>", "Kill terminal" },
			n = { ":FloatermNew<cr>", "New terminal" },
		},
		p = {
			name = "+Projects",
			p = { ":Telescope projects<cr>", "Recent projects" }
		},
        h = { ":lua vim.lsp.buf.hover()<cr>", "View documentation" }
	}, { prefix = "<leader>" })
EOF
