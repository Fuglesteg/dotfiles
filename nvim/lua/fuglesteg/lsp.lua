vim.diagnostic.config({
    virtual_text = true,
    signs = true,
})

vim.opt.signcolumn = 'yes'


local lsp = require('lsp-zero')
lsp.preset('recommended')
-- lsp.preset('lsp-compe')
lsp.set_server_config({
    single_file_support = true
})

lsp.nvim_workspace()

-- TODO: Setup sources
local cmp = require("cmp")
local luasnip = require("luasnip")

local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

vim.opt.completeopt = { "menu", "menuone", "noselect" }
local function superTab(fallback)
    if cmp.visible() then
        cmp.select_next_item()
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
        cmp.select_prev_item()
    elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
    else
        fallback()
    end
end

lsp.setup_nvim_cmp({
    sources = {
        { name = "nvim_lsp" },
        { name = "nvim_lsp_signature_help" },
        { name = "buffer" },
        { name = "path" },
        { name = "luasnip" },
        { name = "nvim_lua" },
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
        ['<Tab>'] = cmp.mapping(function(fallback)
            superTab(fallback)
            if cmp.visible() then
                local entry = cmp.get_selected_entry()
                if not entry then
                      cmp.select_next_item({ behavior = cmp.SelectBehavior.Select })
                else
                      cmp.confirm()
                end
            else
                fallback()
            end
        end, {"i","s","c",}),
        -- ["<Tab>"] = cmp.mapping(superTab, { "i", "s" }),
        ["<S-Tab>"] = cmp.mapping(superSTab, { "i", "s" }),
        ["<C-j>"] = cmp.mapping(superTab, { "i", "s" }),
        ["<C-k>"] = cmp.mapping(superSTab, { "i", "s" }),
    })
})

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
lsp.setup()
