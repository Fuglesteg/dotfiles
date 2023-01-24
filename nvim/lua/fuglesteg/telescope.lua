local telescope = require("telescope")
telescope.load_extension("projects")
telescope.load_extension("file_browser")
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