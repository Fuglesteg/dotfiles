require("project_nvim").setup {
    manual_mode = false,
    detection_methods = { "lsp", "pattern" },
    patterns = { ".git", "package.json" },
    --silent_chdir = false
}
