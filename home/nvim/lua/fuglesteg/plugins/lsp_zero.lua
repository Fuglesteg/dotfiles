return {
    'VonHeikemen/lsp-zero.nvim',
    dependencies = {
        -- LSP Support
        { 'neovim/nvim-lspconfig' },
        { 'williamboman/mason.nvim' },
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
        -- { 'hrsh7th/cmp-nvim-lsp-signature-help' },

        -- Snippets
        { 'L3MON4D3/LuaSnip' },
        -- Snippet Collection (Optional)
        { 'rafamadriz/friendly-snippets' },
    },
    config = function()
        local lsp = require('lsp-zero')

        lsp.preset({
            -- name = 'recommended',
            name = 'minimal',
            set_lsp_keymaps = true,
            suggest_lsp_servers = true,
            manage_nvim_cmp = false,
        })
        -- lsp.preset('lsp-compe')
        lsp.set_server_config({
            single_file_support = true,
        })

        -- OLD Version
        -- lsp.skip_server_setup({"jdtls"})

        -- lsp.nvim_workspace()

        local cmp = require("cmp")
        local luasnip = require("luasnip")

        local has_words_before = function()
            local line, col = unpack(vim.api.nvim_win_get_cursor(0))
            return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
        end

        vim.opt.completeopt = { "menu", "menuone", "noselect" }
        local function superTab(fallback)
            if cmp.visible() then
                cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                -- You could replace the expand_or_jumpable() calls with expand_or_locally_jumpable()
                -- they way you will only jump inside the snippet region
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            elseif has_words_before() then
                cmp.complete()
            else
                fallback()
            end
        end

        local function superSTab(fallback)
            if cmp.visible() then
                cmp.select_prev_item({ behavior = cmp.SelectBehavior.Select })
            elseif luasnip.jumpable( -1) then
                luasnip.jump( -1)
            else
                fallback()
            end
        end
        -- lsp.setup_nvim_cmp(
        local cmp_config = {
            preselect = cmp.PreselectMode.None,
            completion = {
                completeopt = "menu,menuone,noinsert,noselect",
            },
            sources = {
                { name = "nvim_lsp" },
                -- { name = "nvim_lsp_signature_help" }, -- Disabled because Noice has it's own signature help, enabled again, because noice disappears immediately, disabled again, because it isn't better, using own plugin for this (ray-x/lsp_signature.nvim)
                { name = "buffer" },
                { name = "path" },
                { name = "luasnip" },
                { name = "nvim_lua" },
            },
            mapping = cmp.mapping.preset.insert({
                ['<C-s>'] = cmp.mapping.complete(),
                ['<C-space>'] = cmp.mapping.complete(),
                ["<Nul>"] = cmp.mapping.complete(),
                ['<CR>'] = cmp.mapping.confirm({ select = false }),
                ['<Tab>'] = cmp.mapping(superTab, { "i", "s", "c", }),
                ["<S-Tab>"] = cmp.mapping(superSTab, { "i", "s", "c", }),
                ["<C-j>"] = cmp.mapping(superTab, { "i", "s", "c", }),
                ["<C-k>"] = cmp.mapping(superSTab, { "i", "s", "c", }),
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

        cmp.setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
            sources = {
                { name = "dap" },
            }
        })

        lsp.configure("ltex", {
            settings = {
                ltex = {
                    language = "nb-NO"
                }
            }
        })

        -- lsp.configure("jdtls", {
        --     root_dir = require("lspconfig").util.root_pattern(".project", '.gradlew', '.git', 'mvnw'),
        --     settings = {
        --         java = {
        --             checksums = {
        --                 "{'sha256': 'e2b82129ab64751fd40437007bd2f7f2afb3c6e41a9198e628650b22d5824a14', 'allowed': true}",
        --                 {sha256 = 'e2b82129ab64751fd40437007bd2f7f2afb3c6e41a9198e628650b22d5824a14', allowed = true}
        --             }
        --         }
        --     },
        -- })

        lsp.setup()

        vim.diagnostic.config({
            virtual_text = true,
            signs = true,
        })

    end,
}
