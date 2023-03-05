return {
    "folke/noice.nvim",
    opts = {
        presets = {
            command_palette = true,
            bottom_search = true,
            long_message_to_split = true,
            lsp_doc_border = true,
        }
    },
    dependencies = {
        "MunifTanjim/nui.nvim",
        "rcarriga/nvim-notify",
    }
}
