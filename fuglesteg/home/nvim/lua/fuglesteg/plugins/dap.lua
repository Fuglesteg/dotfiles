return {
    "mfussenegger/nvim-dap",
    config = function()
        local dap = require("dap")
        require("dap-go").setup()
    end,
}
