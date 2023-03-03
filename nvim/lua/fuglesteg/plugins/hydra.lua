return {
    "anuvyklack/hydra.nvim",
    config = function()
        local hydra = require("hydra")
        local cmd = require("hydra.keymap-util").cmd
        local buffer = require("fuglesteg.buffer")

        hydra({
            name = "Buffer",
            mode = "n",
            body = "<leader>b",
            heads = {
                { "n", cmd "bnext",                        { desc = "Next buffer" } },
                { "N", cmd "bprev",                        { desc = "Previous buffer" } },
                { "d", function() buffer.kill(0, nil) end, { desc = "Close buffer" } },
            },
            hint = [[Buffers]],
            config = {
                silent = true,
                -- invoke_on_body = true,
                hint = {
                    border = "double",
                    -- show_name = false,
                    position = "bottom",
                    type = "window",
                }
            }
        })

        local sSplit = require("smart-splits")

        hydra({
            name = "Window",
            mode = "n",
            body = "<leader>ww",
            heads = {
                { "h",     "<c-w>h",                               { desc = "Window left" } },
                { "l",     "<c-w>l",                               { desc = "Window right" } },
                { "j",     "<c-w>j",                               { desc = "Window down" } },
                { "k",     "<c-w>k",                               { desc = "Window up" } },
                { "T",     "<c-w>T",                               { desc = "Send to new tab" } },
                { "<c-h>", function() sSplit.resize_left(2) end,   { desc = "Change width left" } },
                { "<c-l>", function() sSplit.resize_right(2) end,  { desc = "Change width right" } },
                { "<c-j>", function() sSplit.resize_down(2) end,   { desc = "Change height down" } },
                { "<c-k>", function() sSplit.resize_up(2) end,     { desc = "Change height up" } },
                { "=",     "<c-w>=",                               { desc = "Equalize windows" } },
                { "d",     "<c-w>c",                               { desc = "Close window" } },
                { "v",     "<c-w>v",                               { desc = "Vertical split" } },
                { "s",     "<c-w>s",                               { desc = "Split" } },
            },
            hint = [[Windows]],
            config = {
                invoke_on_body = true,
                silent = true,
                -- invoke_on_body = true,
                hint = {
                    border = "double",
                    -- show_name = false,
                    position = "bottom",
                    type = "window",
                }
            }
        })

        hydra({
            name = "Scroll",
            mode = "n",
            body = "z",
            heads = {
                { "h", "zh:IndentBlanklineRefresh", { desc = "Scroll to the left" } },
                { "l", "zl:IndentBlanklineRefresh", { desc = "Scroll to the right" } },
                { "j", "<c-e>",                     { desc = "Scroll up" } },
                { "k", "<c-y>",                     { desc = "Scroll down" } },
            }
        })

        hydra({
            name = "Debug",
            mode = "n",
            body = "<leader>d",
            heads = {
                { "i", require("dap").step_into,         { desc = "Step into" } },
                { "o", require("dap").step_over,         { desc = "Step over" } },
                { "s", require("dap").continue,          { desc = "Start or continue" } },
                { "b", require("dap").toggle_breakpoint, { desc = "Toggle breakpoint" } },
            },
            config = {
                invoke_on_body = true,
            }
        })
    end
}
