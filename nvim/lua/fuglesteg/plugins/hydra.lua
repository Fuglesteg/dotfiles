return {
    "anuvyklack/hydra.nvim",
    dependencies ={ "echasnovski/mini.bufremove" },
    config = function()
        local hydra = require("hydra")
        local cmd = require("hydra.keymap-util").cmd
        -- local buffer = require("fuglesteg.buffer")
        local function status_line_update()
            -- require("lualine").refresh()
        end

        local function buffernext()
            vim.cmd.bnext()
        end

        local function bufferprev()
            vim.cmd.bprev()
        end

        hydra({
            name = "Buffer",
            mode = "n",
            body = "<leader>b",
            heads = {
                { "n", buffernext, { desc = "Next buffer" } },
                { "N", bufferprev, { desc = "Previous buffer" } },
                { "d", MiniBufremove.delete, { desc = "Close buffer" } },
                { "c", cmd "new", { desc = "New buffer" } },
                { "b", cmd "Telescope buffers", { desc = "Search buffers" } },
            },
            hint = [[Buffers]],
            config = {
                silent = true,
                color = "pink",
                -- invoke_on_body = true,
                hint = {
                    border = "double",
                    -- show_name = false,
                    position = "bottom",
                    type = "window",
                },
                on_exit = status_line_update,
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
                 { "h", function() vim.api.nvim_input_mouse("wheel", "left", "", 0, 0, 0) end, { desc = "Scroll to the left" } },
                 { "l", function() vim.api.nvim_input_mouse("wheel", "right", "", 0, 0, 0) end, { desc = "Scroll to the right" } },
                 { "j", "<c-e>",                     { desc = "Scroll up" } },
                 { "k", "<c-y>",                     { desc = "Scroll down" } },
             },
             config = {
                color = "pink",
                 hint = {
                     position = "top",
                     type = "window",
                 }
             },
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
                 color = "pink",
                 hint = {
                     position = "top",
                     type = "window",
                 }
             }
         })
    end
}
