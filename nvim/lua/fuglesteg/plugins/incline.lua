return {
    "b0o/incline.nvim",
    event = "BufReadPre",
    config = function()
        local colors = require("tokyonight.colors").setup()
        require("incline").setup({
            render = function(props)
                local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ":t")
                local icon, color = require("nvim-web-devicons").get_icon_color(filename)
                return { { icon, guifg = color }, { " " }, { filename } }
            end,
            highlight = {
                groups = {
                    InclineNormal = { guifg = colors.blue, guibg = colors.bg_dark},
                    InclineNormalNC = { guibg = colors.bg, guifg = colors.blue}
                }
            },
            hide = {
                cursorline = true,
                only_win = true,
            },
            window = {
                placement = {
                    -- horizontal = "center"
                },
                margin = {
                    -- horizontal = 0,
                    vertical = 0,
                }
            }
        })
    end,
}
