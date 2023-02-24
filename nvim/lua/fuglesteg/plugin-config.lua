require("fuglesteg.lsp_zero")
require("fuglesteg.nvim-dap")
require("possession").setup({
    autosave = {
        current = true,
        tmp = true,
    }
})

-- FIXME: Noice and dressing overriding each other

-- require("noice").setup({})

require("fuglesteg.hydra")

-- require("dressing").setup({
--     input = {
--         enabled = true
--     }
-- })

require("indent_blankline").setup {
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = true,
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

require("fuglesteg.markdown-preview")
require("fuglesteg.lua-line")
require('todo-comments').setup {}
require('trouble').setup {}

-- require("fuglesteg.project-nvim")
require("fuglesteg.telescope")
require("fuglesteg.nvim-tree")
