return {
    "nvim-lualine/lualine.nvim",
    config = function()
        local function session_name()
            return require("possession.session").get_session_name() or ""
        end

        local function show_buffers()
            local hydra_name = require("hydra.statusline").get_name()
            return hydra_name == "Buffer"
        end

        local function show_tabs()
            return #vim.api.nvim_list_tabpages() > 1
        end

        local function should_show_tabline()
            -- print("Show buffer: ", show_buffers())
            -- print("Show tabs: ", show_tabs())
            return show_tabs()
        end

        local tabline_value = vim.opt.showtabline
        vim.opt.showtabline = 0
        local function show_tabline_if()
            if should_show_tabline() then
                vim.opt.showtabline = tabline_value
            else
                vim.opt.showtabline = 0
            end
        end

        local theme = "tokyonight"
        local lualine_theme = require("lualine.themes." .. theme)
        local bg_color = lualine_theme.visual.b.bg
        local fg_color = lualine_theme.normal.b.fg

        require("lualine").setup {
            options = {
                theme = theme,
                component_separators = "|",
                -- component_separators = "",
                section_separators = { left = "", right = "" },
            },
            sections = {
                lualine_a = { "mode" },
                lualine_b = {
                },
                lualine_c = {
                    {
                        "filename",
                        path = 1,
                        color = { bg = bg_color, fg = fg_color },
                        separator = { left = "", right = "" },
                    },
                    {
                        "filetype",
                        icon_only = true,
                        color = { bg = bg_color, fg = fg_color },
                        separator = { left = "", right = "" },
                    },
                    "diff", "diagnostics",
                },
                lualine_x = {
                    {
                        session_name,
                        icon = "",
                    },
                    "branch"
                },
                lualine_y = {
                    function()
                        local recording_register = vim.fn.reg_recording()
                        if recording_register == "" then
                            return ""
                        else
                            return "Recording @" .. recording_register
                        end
                    end
                }
            },
            tabline = {
                lualine_a = {
                    {
                        "tabs",
                        max_length = vim.o.columns,
                        mode = 3,
                        cond = show_tabs,
                    },
                    function()
                        show_tabline_if()
                        return ""
                    end
                },
            },
            winbar = {
                lualine_a = {
                    {
                        "buffers",
                        cond = show_buffers,
                    },
                }
            }
        }
    end,
}
