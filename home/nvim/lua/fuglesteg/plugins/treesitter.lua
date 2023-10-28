return {
    'nvim-treesitter/nvim-treesitter',
    config = function()
        require("nvim-treesitter.configs").setup {
            highlight = {
                enable = true,
                additional_vim_regex_highlighting = false,
            },
            indent = {
                enable = true
            }
        }
        -- vim.cmd([[
        -- set foldmethod=expr
        -- set foldexpr=nvim_treesitter#foldexpr()
        -- set nofoldenable
        -- ]])
    end
}
