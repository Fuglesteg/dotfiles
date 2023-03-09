return {
    'petertriho/nvim-scrollbar',
    config = function ()
        local colors = require("tokyonight.colors").setup()
        require("scrollbar").setup({
            handle = {
                color = colors.bg_highlight,
            },
            marks = {
                Search = { color = colors.orange },
                Error = { color = colors.error },
                Warn = { color = colors.warning },
                Info = { color = colors.info },
                Hint = { color = colors.hint },
                Misc = { color = colors.purple },
            },
            handlers = {
                cursor = false,
                gitsigns = true,
                search = true,
            },
            hide_if_all_visible = true,
        })
        require("scrollbar.handlers.gitsigns").setup()
        require("scrollbar.handlers.search").setup({
            override_lens = function() end
        })
    end,
    dependencies = {
        "lewis6991/gitsigns.nvim",
        "kevinhwang91/nvim-hlslens",
    }
}
