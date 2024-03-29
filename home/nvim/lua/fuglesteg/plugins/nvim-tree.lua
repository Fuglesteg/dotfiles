return {
    'kyazdani42/nvim-tree.lua',
    config = function()
        vim.g.nvim_tree_respect_buf_cwd = 1
        require("nvim-tree").setup({
            update_cwd = true,
            update_focused_file = {
                enable = true,
                update_cwd = true,
            },
            actions = {
                change_dir = {
                    global = true
                }
            }
        })
    end
}
