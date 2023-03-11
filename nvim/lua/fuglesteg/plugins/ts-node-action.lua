return {
    'ckolkey/ts-node-action',
     dependencies = { 'nvim-treesitter' },
    config = function()
        local null_ls = require("null-ls")
        null_ls.register({
            name = "more_actions",
            method = { null_ls.methods.CODE_ACTION },
            filetypes = { "_all" },
            generator = {
                fn = require("ts-node-action").available_actions,
            },
        })
    end
}
