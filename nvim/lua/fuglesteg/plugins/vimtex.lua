return {
    "lervag/vimtex",
    config = function()
        vim.g.vimtex_view_method = "zathura"
        vim.b.spell = true
        vim.b.spelllang = "nb"
        vim.b.textwidth = 80
    end,
    ft = "tex"
}
