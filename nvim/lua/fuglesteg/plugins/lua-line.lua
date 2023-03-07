return {
    "nvim-lualine/lualine.nvim",
    config = function()
        local function status_clock()
            local timeTable = os.date("*t")
            local timeHourMinute = (timeTable.hour .. ":" .. timeTable.min) or ""
            return timeHourMinute
        end

        local function session_name()
            return require("possession.session").session_name or ""
        end

        local theme = "tokyonight"
        local lualine_theme = require("lualine.themes." .. theme)
        local bg_color = lualine_theme.visual.b.bg
        local fg_color = lualine_theme.normal.b.fg

        local tabline_value = vim.opt.showtabline
        require("lualine").setup {
            options = {
                theme = theme,
                component_separators = "|",
                section_separators = { left = "", right = "" },
            },
            sections = {
                lualine_a = { "mode" },
                lualine_b = {
                    -- {
                    --     -- "branch",
                    --     -- separator = { left = "", right = "" },
                    -- }
                },
                lualine_c = {
                    {
                        "filename",
                        color = { bg = bg_color, fg = fg_color },
                        separator = { left = "", right = "" },
                    },
                    {
                        "filetype",
                        icon_only = true,
                        color = { bg = bg_color, fg = fg_color },
                        separator = { left = "", right = "" },
                    },
                    "diff", "diagnostics",
                    function()
                        local navic = require("nvim-navic")
                        if navic.is_available() then
                            return navic.get_location()
                        else
                            return ""
                        end
                    end,
                    -- function()
                    --     return "%="
                    -- end,
                },
                lualine_x = {
                    session_name,
                    "branch"
                },
            },
            tabline = {
                lualine_a = {
                    {
                        "tabs",
                        max_length = vim.o.columns,
                        mode = 3,
                        cond = function()
                            if #vim.api.nvim_list_tabpages() > 1 then
                                vim.opt.showtabline = tabline_value
                                return true
                            else
                                vim.opt.showtabline = 0
                                return false
                            end
                        end
                    },
                }
            }
        }
    end,
}
