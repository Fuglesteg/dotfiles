return {
    "mfussenegger/nvim-dap",
    dependencies = { "jay-babu/mason-nvim-dap.nvim", "williamboman/mason.nvim" },
    config = function()
        local dap = require("dap")
        require("mason-nvim-dap").setup({
            handlers = {
                function(config)
                    -- Keep original functionality
                    require('mason-nvim-dap').default_setup(config)
                end,
                firefox = function(config)
                    config.configurations = {
                        {
                            name = 'Firefox: Debug',
                            type = 'firefox',
                            request = 'launch',
                            reAttach = true,
                            firefoxExecutable = vim.fn.exepath('firefox'),
                            url = "dev.minidrett.no",
                            webRoot = "${workspaceFolder}/src",
                            pathMappings = {
                                {
                                    url = "webpack://nif/${workspaceFolderBasename}/src",
                                    path = "${webRoot}"
                                }
                            },
                            sourceMaps = true
                        }
                    }
                    config.filetypes = 	{ 'javascriptreact', 'typescriptreact', 'typescript', 'javascript', 'vue' }
                    require('mason-nvim-dap').default_setup(config) -- don't forget this!
                end,
            }
        })
    end
}
