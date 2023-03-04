return {
    'TimUntersberger/neogit',
    dependencies = {
        'sindrets/diffview.nvim', dependencies = 'nvim-lua/plenary.nvim'
    },
    config = function()
        require("neogit").setup({
            integrations = {
                diffview = true
            }
        })
    end
}
