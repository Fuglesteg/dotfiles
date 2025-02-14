return {
    'neovim/nvim-lspconfig',
    dependencies = {
        -- LSP installation
        { 'williamboman/mason.nvim', opts = {} },
        { 'williamboman/mason-lspconfig.nvim' },

        -- Autocompletion
        { 'hrsh7th/nvim-cmp' },
        { 'hrsh7th/cmp-buffer' },
        { 'hrsh7th/cmp-path' },
        { 'hrsh7th/cmp-cmdline' },
        { 'saadparwaiz1/cmp_luasnip' },
        { 'hrsh7th/cmp-nvim-lsp' },
        { 'hrsh7th/cmp-nvim-lua' },
        { "rcarriga/cmp-dap" },
        { "GustavEikaas/easy-dotnet.nvim" },

        -- Useful status updates for LSP.
        { 'j-hui/fidget.nvim', opts = {} },

        -- Snippets
        { 'L3MON4D3/LuaSnip' },
        -- Snippet Collection (Optional)
        { 'rafamadriz/friendly-snippets' },
    },
    config = function()
        local cmp = require("cmp")
        require("luasnip.loaders.from_vscode").lazy_load()
        local luasnip = require("luasnip")

        local has_words_before = function()
            local line, col = unpack(vim.api.nvim_win_get_cursor(0))
            return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
        end

        vim.opt.completeopt = { "menu", "menuone", "noselect" }

        local function next_item()
            cmp.complete()
            cmp.select_next_item({behavior = cmp.SelectBehavior.Select})
        end

        local function prev_item()
            cmp.complete()
            cmp.select_prev_item({behavior = cmp.SelectBehavior.Select})
        end

        cmp.register_source("easy-dotnet", require("easy-dotnet").package_completion_source)

        local cmp_config = {
            preselect = cmp.PreselectMode.None,
            completion = {
                completeopt = "menu,menuone,noinsert,noselect",
                autocomplete = false
            },
            sources = {
                { name = "nvim_lsp" },
                { name = "luasnip" },
                { name = "buffer" },
                { name = "path" },
                { name = "nvim_lua" },
            },
            mapping = cmp.mapping.preset.insert({
                ["<C-y>"] = cmp.mapping.confirm({ select = false }),
                ["<C-n>"] = next_item,
                ["<C-p>"] = prev_item,
            }),
            snippet = {
                expand = function(args)
                    require('luasnip').lsp_expand(args.body)
                end,
            },
            -- Formatting of completion menu, adding symbols like vscode
            formatting = {
                format = function(entry, vim_item)
                    if vim.tbl_contains({ 'path' }, entry.source.name) then
                        local icon, hl_group = require('nvim-web-devicons').get_icon(entry:get_completion_item().label)
                        if icon then
                            vim_item.kind = icon
                            vim_item.kind_hl_group = hl_group
                            return vim_item
                        end
                    end
                    return require('lspkind').cmp_format({ with_text = false })(entry, vim_item)
                end
            },
            experimental = { ghost_text = true },
        }
        cmp.setup(cmp_config)

        cmp.setup.cmdline({ "/", "?" }, {
            mapping = cmp.mapping.preset.cmdline(),
            sources = {
                { name = "buffer" }
            }
        })
        cmp.setup.cmdline(":", {
            mapping = cmp.mapping.preset.cmdline(),
            sources = cmp.config.sources({
                { name = "path" }
            },
            {
                {
                    name = "cmdline",
                    option = {
                        ignore_cmds = { "Man", "!" }
                    }
                }
            })
        })



        -- Completion in command line buffer (<C-f>)
        cmp.setup.filetype("vim", {
            mapping = cmp.mapping.preset.cmdline(),
            sources = cmp.config.sources({
                { name = "path" }
            },
                {
                    {
                        name = "cmdline",
                        option = {
                            ignore_cmds = { "Man", "!" }
                        }
                    }
                })
        })

        cmp.setup.filetype({ "xml" }, {
            sources = {
                { name = "easy-dotnet" },
                { name = "buffer" },
                { name = "path" },
            }
        })

        cmp.setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
            sources = {
                { name = "dap" },
            }
        })

        vim.diagnostic.config({
            virtual_text = true,
            signs = true,
        })

        -- Gutter signs
        local signs = { ERROR = '', WARN = '', INFO = '', HINT = '' }
        local diagnostic_signs = {}
        for type, icon in pairs(signs) do
            diagnostic_signs[vim.diagnostic.severity[type]] = icon
        end
        vim.diagnostic.config { signs = { text = diagnostic_signs } }

        local capabilities = vim.lsp.protocol.make_client_capabilities()
        capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

        -- LSP Server settings
        local servers = {}

        -- Servers that are configured elsewhere
        local disabledServerConfigs = {
            omnisharp = true
        }

        require('mason-lspconfig').setup({
            handlers = {
                function(server_name)
                    -- if disabledServerConfigs[server_name] ~= nil then
                    --     return
                    -- end
                    local server = servers[server_name] or {}
                    server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
                    require('lspconfig')[server_name].setup(server)
                end,
            }
        })
    end,
}
