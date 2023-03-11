return {
    "HiPhish/nvim-ts-rainbow2",
    enabled = false,
    dependency = "nvim-treesitter/nvim-treesitter",
    config = function()
        require("nvim-treesitter.configs").setup({
            rainbow = {
                enable = true,
                query = "rainbow-parens",
                strategy = require("ts-rainbow.strategy.global")
            }
        })
    end
}
