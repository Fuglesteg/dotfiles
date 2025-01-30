return {
    "Hoffs/omnisharp-extended-lsp.nvim",
    ft = "csharp",
    config = function()
        local omni_ext = require('omnisharp_extended')
        vim.keymap.set("n", "gd", omni_ext.telescope_lsp_definition)
        vim.keymap.set("n", "gD", omni_ext.telescope_lsp_type_definition)
        vim.keymap.set("n", "gr", omni_ext.telescope_lsp_references)
        vim.keymap.set("n", "gi", omni_ext.telescope_lsp_implementation)
    end
}
