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

        local function update()
            require("lualine").refresh()
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
            return show_buffers() or show_tabs()
        end

        local tabline_value = vim.opt.showtabline
        vim.opt.showtabline = 0
        local function show_tabline_if()
            if should_show_tabline() then
                vim.opt.showtabline = 2
            else
                vim.opt.showtabline = 0
            end
        end

        local theme = require("lualine.themes.tokyonight")
        local bg_color = theme.visual.b.bg
        local fg_color = theme.normal.b.fg

        require("lualine").setup {
            options = {
                theme = "tokyonight",
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
                        mode = 3,
                        cond = show_tabs,
                    },
                },
                lualine_b = {
                    {
                        "buffers",
                        cond = show_buffers,
                    },
                    function()
                        show_tabline_if()
                        return ""
                    end
                }
            }
        }
    end,
}
