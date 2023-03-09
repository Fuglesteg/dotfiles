return {
    "b0o/incline.nvim",
    event = "BufReadPre",
    config = function()
        local colors = require("tokyonight.colors").setup()
        require("incline").setup({
            highlight = {
                groups = {
                    InclineNormal = { guibg = colors.blue, guifg = colors.black},
                    InclineNormalNC = { guifg = colors.blue, guibg = colors.black}
                }
            },
            render = function(props)
                local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ":t")
                local icon, color = require("nvim-web-devicons").get_icon_color(filename)
                return { { icon, guifg = color }, { " " }, { filename } }
            end,
            hide = {
                cursorline = true,
                only_win = true,
            },
            window = {
                placement = {
                    -- horizontal = "center"
                }
            }
        })
    end,
}
