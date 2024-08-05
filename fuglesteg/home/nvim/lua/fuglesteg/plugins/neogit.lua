return {
    'TimUntersberger/neogit',
    dependencies = {
        'sindrets/diffview.nvim',
    },
    version = "0.0.1",
    tag = "v0.0.1",
    config = function()
        require("neogit").setup({
            integrations = {
                diffview = true
            }
        })
    end
}
