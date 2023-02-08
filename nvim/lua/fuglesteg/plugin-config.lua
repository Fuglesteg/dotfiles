require("fuglesteg.lsp_zero")
require("fuglesteg.nvim-dap")

-- FIXME: Noice and dressing overriding each other
-- require("noice").setup({})

require("fuglesteg.hydra")

-- require("dressing").setup({
--     input = {
--         enabled = true
--     }
-- })

require("indent_blankline").setup { }

require("neogit").setup({
    integrations = {
        diffview = true
    }
})

-- Treesitter
require("nvim-treesitter.configs").setup {
    highlight = {
        enable = true
    }
}

-- MarkdownPreview
vim.g.mkdp_open_to_the_world = 1
vim.g.mkdp_open_ip = "127.0.0.1"
vim.g.mkdp_port = 8080

require("fuglesteg.lua-line")
require('todo-comments').setup {}
require('trouble').setup {}

require("fuglesteg.project-nvim")
require("fuglesteg.telescope")
require("fuglesteg.nvim-tree")
