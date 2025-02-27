return {
    'nvim-treesitter/nvim-treesitter',
    config = function()
        require("nvim-treesitter.configs").setup {
            highlight = {
                enable = true,
                additional_vim_regex_highlighting = false,
                disable = { "vue" } -- Disable in vue because it causes crash smh
            },
            indent = {
                enable = true
            }
        }
        require("nvim-treesitter.install").prefer_git = true
    end
}
