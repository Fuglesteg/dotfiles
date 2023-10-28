return {
    "SmiteshP/nvim-navic",
    config = function()
        local navic = require("nvim-navic")
        navic.setup({
            highlight = true,
        })
        local lsp = require("lsp-zero")
        lsp.on_attach(function(client, bufnr)
                if client.server_capabilities.documentSymbolProvider then
                    navic.attach(client, bufnr)
                end
            end
        )
    end
}
