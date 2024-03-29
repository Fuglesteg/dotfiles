return {
    "folke/noice.nvim",
    enabled = false,
    opts = {
        lsp = {
            override = {
                ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                ["vim.lsp.util.stylize_markdown"] = true,
                ["cmp.entry.get_documentation"] = true,
            },
            signature = {
                enabled = false,
            },
        },
        popupmenu = {
        },
        smart_move = {
            excluded_filetypes = { "notify" }
        },
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
