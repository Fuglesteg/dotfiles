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

-- Code intelligence
    use 'nvim-treesitter/nvim-treesitter'
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
    use { "rcarriga/nvim-dap-ui", requires = {"mfussenegger/nvim-dap"} }
-- Formatting
    use "tpope/vim-sleuth"

    -- Java
    -- use "mfussenegger/nvim-jdtls"


-- Git
    use {'TimUntersberger/neogit'}
    use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }

-- Utility
    use 'amadeus/vim-convert-color-to'
    use 'tpope/vim-surround'
    use 'tpope/vim-commentary'
    use  "nvim-telescope/telescope-file-browser.nvim"
    use 'kyazdani42/nvim-tree.lua'
    use 'mrjones2014/smart-splits.nvim'
-- Search
    use 'nvim-telescope/telescope.nvim'
    use 'folke/trouble.nvim'
    use 'folke/todo-comments.nvim'
    use {'nvim-telescope/telescope-fzf-native.nvim',
        run = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && \
            cmake --build build --config Release && \
            cmake --install build --prefix build'
        }

-- Keybinds
    use 'folke/which-key.nvim'
    use "anuvyklack/hydra.nvim"

-- Visual
    use 'nvim-lualine/lualine.nvim'
    use 'onsails/lspkind.nvim'
    use 'kyazdani42/nvim-web-devicons'
    use "lukas-reineke/indent-blankline.nvim"
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
-- Colorschemes
    use 'romgrk/doom-one.vim'
    use 'folke/tokyonight.nvim'

-- Plugin dependency
    use 'nvim-lua/plenary.nvim'


    use 'voldikss/vim-floaterm'

    -- install without yarn or npm
    -- use({
    --     "iamcco/markdown-preview.nvim",
    --     run = function() vim.fn["mkdp#util#install"]() end,
    -- })

    use({ "iamcco/markdown-preview.nvim", run = "cd app && npm install", setup = function() vim.g.mkdp_filetypes = { "markdown" } end, ft = { "markdown" }, })
end)
