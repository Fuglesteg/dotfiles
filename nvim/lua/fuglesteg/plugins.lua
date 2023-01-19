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
        {'hrsh7th/cmp-cmdline'},
        {'saadparwaiz1/cmp_luasnip'},
        {'hrsh7th/cmp-nvim-lsp'},
        {'hrsh7th/cmp-nvim-lua'},
        {'hrsh7th/cmp-nvim-lsp-signature-help'},

        -- Snippets
        {'L3MON4D3/LuaSnip'},
        -- Snippet Collection (Optional)
        {'rafamadriz/friendly-snippets'},
        }
    }
    use({  "folke/noice.nvim",
        config = function()
            require("noice").setup({
                -- add any options here    
            })
        end,
        requires = {
            "MunifTanjim/nui.nvim",
            "rcarriga/nvim-notify",
        }
    })

    use {'stevearc/dressing.nvim'}
    use {'TimUntersberger/neogit'}
    use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }
    use 'amadeus/vim-convert-color-to'
    use 'tpope/vim-surround'
    use "nvim-telescope/telescope-file-browser.nvim"
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
