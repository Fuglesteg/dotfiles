return {
    'nvim-telescope/telescope.nvim',
    dependencies = {
        {
            'nvim-telescope/telescope-fzf-native.nvim',
            build = [[cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && \
                cmake --build build --config Release && \
                cmake --install build --prefix build]]
        }
    },
    config = function()
        local telescope = require("telescope")
        telescope.setup {
            defaults = {
                file_ignore_patterns = {
                    "%.png",
                    "%.jpg",
                    "%.jpeg",
                    "%.gif"
                }
            }
        }
        telescope.load_extension("file_browser")
        telescope.load_extension("fzf")
        telescope.load_extension("possession")
    end,
}
