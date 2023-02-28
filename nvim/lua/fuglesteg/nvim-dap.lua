local dap, dapui = require("dap"), require("dapui")
dapui.setup()

dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
end

dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
end

dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close()
end

require("dap-go").setup()
-- OLD vscode-go based dap config for go
-- dap.adapters.go = {
--     type = 'executable';
--     command = 'node';
--     args = { os.getenv('HOME') .. '/dev/golang/vscode-go/dist/debugAdapter.js' };
-- }

-- dap.configurations.go = {
--     {
--         type = 'go';
--         name = 'Debug';
--         request = 'launch';
--         showLog = false;
--         program = "${file}";
--         dlvToolPath = vim.fn.exepath('dlv') -- Adjust to where delve is installed
--     },
-- }
