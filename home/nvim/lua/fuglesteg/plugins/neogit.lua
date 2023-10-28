return {
    'TimUntersberger/neogit',
    dependencies = {
        'sindrets/diffview.nvim',
    },
    config = function()
        require("neogit").setup({
            integrations = {
                diffview = true
            }
        })
    end
}
