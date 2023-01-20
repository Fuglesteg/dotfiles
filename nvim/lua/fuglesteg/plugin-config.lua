require("fuglesteg.lsp_zero")

-- FIXME: Noice and dressing overriding each other
require("noice").setup({})

require("dressing").setup({
    input = {
        enabled = true
    }
})

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

require("fuglesteg.lua-line")
require('todo-comments').setup {}
require('trouble').setup {}

require("fuglesteg.project-nvim")
require("fuglesteg.telescope")
require("fuglesteg.nvim-tree")
