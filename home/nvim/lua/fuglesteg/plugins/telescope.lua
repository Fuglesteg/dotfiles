return {
    'nvim-telescope/telescope.nvim',
    dependencies = {
        {
            'nvim-telescope/telescope-fzf-native.nvim',
            build = "make"
        }
    },
    config = function()
        local telescope = require("telescope")
        telescope.setup {
            defaults = {
                layout_config = {
                    height = 0.9,
                    width = 0.9,
                    prompt_position = "top"
                },
                sorting_strategy = "ascending",
                file_ignore_patterns = {
                    "%.png",
                    "%.jpg",
                    "%.jpeg",
                    "%.gif"
                },
                mappings = {
                    n = {
                        ["<c-c>"] = require("telescope.actions").delete_buffer
                    },
                    i = {
                        ["<c-c>"] = require("telescope.actions").delete_buffer
                    }
                }
            },
        }
        telescope.load_extension("fzf")
        telescope.load_extension("possession")
    end,
}
