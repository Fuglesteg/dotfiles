require("fuglesteg.lsp_zero")

-- Noice
require("noice").setup({})

require("neogit").setup({
    integrations = {
        diffview = true
    }
})
require("dressing").setup({
    input = {
        enabled = true
    }
})

-- Treesitter
require("nvim-treesitter.configs").setup {
    highlight = {
        enable = true
    }
}

-- FIXME: Fix status clock or remove
-- Lua line config
local function statusClock()
    local timeTable = date()
    local timeHourMinute = timeTable.hour
    return timeHourMinute
end

require('todo-comments').setup {}
require('trouble').setup {}
require('lualine').setup {
    options = { theme = 'tokyonight' },
    sections = { lualine_a = { statusClock } }
}

-- Config for project.nvim and nvim-tree
-- Vim Script
vim.api.nvim_set_var('nvim_tree_respect_buf_cwd', 1)

require("project_nvim").setup {
    manual_mode = false,
    detection_methods = { "lsp", "pattern" },
    patterns = { ".git", "package.json" },
    --silent_chdir = false
}
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

