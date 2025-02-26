return {
    'neovim/nvim-lspconfig',
    dependencies = {
        -- LSP installation
        { 'williamboman/mason.nvim', opts = {} },
        { 'williamboman/mason-lspconfig.nvim' },

        -- Autocompletion
        { 
            "saghen/blink.cmp",
            version = "v0.12.4", -- Use release to get prebuilt binaries
            opts = {
                keymap = {
                    preset = "default",
                    ["<C-n>"] = {
                        "show",
                        "select_next"
                    },
                    ["<C-p>"] = {
                        "show",
                        "select_prev"
                    }
                },
                appearance = {
                    -- Sets the fallback highlight groups to nvim-cmp's highlight groups
                    -- Useful for when your theme doesn't support blink.cmp
                    -- Will be removed in a future release
                    use_nvim_cmp_as_default = true,
                    -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
                    -- Adjusts spacing to ensure icons are aligned
                    nerd_font_variant = 'mono',
                },
                completion = {
                    menu = {
                        auto_show = false
                    },
                    ghost_text = { enabled = true },
                    documentation = {
                        auto_show = true,
                        auto_show_delay_ms = 500,
                    }
                },
                fuzzy = {
                    prebuilt_binaries = {
                        download = false
                    }
                },

                -- Default list of enabled providers defined so that you can extend it
                -- elsewhere in your config, without redefining it, due to `opts_extend`
                sources = {
                    default = { 'lsp', 'path', 'snippets', 'buffer' },
                },
                cmdline = {
                    enabled = true,
                    keymap = nil, -- Inherits from top level `keymap` config when not set
                    sources = function()
                        local type = vim.fn.getcmdtype()
                        -- Search forward and backward
                        if type == '/' or type == '?' then return { 'buffer' } end
                        -- Commands
                        if type == ':' or type == '@' then return { 'cmdline' } end
                        return {}
                    end,
                    completion = {
                        trigger = {
                            show_on_blocked_trigger_characters = {},
                            show_on_x_blocked_trigger_characters = nil, -- Inherits from top level `completion.trigger.show_on_blocked_trigger_characters` config when not set
                        },
                        menu = {
                            auto_show = nil, -- Inherits from top level `completion.menu.auto_show` config when not set
                            draw = {
                                columns = { { 'label', 'label_description', gap = 1 } },
                            },
                        }
                    }
                }
            },
            opts_extend = {
                "sources.default"
            }
        },
        { "GustavEikaas/easy-dotnet.nvim" },

        -- Useful status updates for LSP.
        { 'j-hui/fidget.nvim', opts = {} },

        -- Snippets
        { 'L3MON4D3/LuaSnip' },
        -- Snippet Collection (Optional)
        { 'rafamadriz/friendly-snippets' },
    },
    config = function()
        vim.opt.completeopt = { "menu", "menuone", "noselect" }

        local blink = require("blink.cmp")

        -- Completion in command line buffer (<C-f>)
        -- DAP

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

        capabilities = blink.get_lsp_capabilities()

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
