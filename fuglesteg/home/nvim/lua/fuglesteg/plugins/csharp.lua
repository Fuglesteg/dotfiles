return {
    "iabdelkareem/csharp.nvim",
    dependencies = {
        "williamboman/mason.nvim", -- Required, automatically installs omnisharp
        "mfussenegger/nvim-dap",
        "Tastyep/structlog.nvim", -- Optional, but highly recommended for debugging
    },
    ft = "cs",
    config = function ()
        require("mason").setup() -- Mason setup must run before csharp, only if you want to use omnisharp
        local csharp = require("csharp")
        csharp.setup({
            lsp = {
                omnisharp = {
                    enable_analyzers_support = false
                }
            }
        })

        vim.keymap.set("n", "gd", csharp.go_to_definition, { desc = "Go to definition" })
        vim.keymap.set("n", "<leader>ds", csharp.debug_project, { desc = "Debug project" })
    end
}
