require("fuglesteg.lsp_zero")
require("fuglesteg.nvim-dap")

-- FIXME: Noice and dressing overriding each other
require("noice").setup({})

require("dressing").setup({
    input = {
        enabled = true
    }
})

require("indent_blankline").setup {
    
}

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
