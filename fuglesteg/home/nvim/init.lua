--------------------------
--== General settings ==--
--------------------------
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.wrap = false
vim.opt.smartindent = true
vim.opt.cursorline = false
vim.opt.signcolumn = "yes"
vim.diagnostic.config({ virtual_text = true })
vim.opt.complete = "o"
vim.opt.listchars = "leadmultispace:   ,multispace:   ,tab: ,nbsp:,trail: "
vim.opt.list = true
vim.opt.conceallevel = 2

vim.opt.scrolloff = 8

vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Appearance
vim.opt.termguicolors = true
vim.cmd("colorscheme habamax")
vim.api.nvim_set_hl(0, "Comment", { fg = "#dece3c", italic = true })
vim.api.nvim_set_hl(0, "Normal", { fg = "#dbdbdb", bg = "#0a0a0a" })
vim.api.nvim_set_hl(0, "Identifier", { fg = "#dbdbdb" })
vim.api.nvim_set_hl(0, "Special", { fg = "#93979e" })
vim.api.nvim_set_hl(0, "DiagnosticError", { fg = "#ff6363", italic = true, bold = false })
vim.api.nvim_set_hl(0, "Pmenu", { bg = "#121212" })
vim.api.nvim_set_hl(0, "PmenuKind", { bg = "#121212" })

-- Ignore files for vimgrep, etc.
vim.g.wildignore = "bin/**,node_modules/**,*.o,*.obj,*.dll,*.svg,*.png,*.jpg"

------------------
--== Keybinds ==--
------------------

vim.g.mapleader = ' '
vim.g.maplocalleader = ','

local function nmap(binding, action, description)
    vim.keymap.set('n', binding, action, { desc = description })
end

local function vmap(binding, action, description)
    vim.keymap.set('v', binding, action, { desc = description })
end

-- General
nmap("<Esc>", vim.cmd.nohlsearch)
vmap("<", "<gv")
vmap(">", ">gv")
nmap("<Leader>ff", vim.cmd.Oil)
nmap("<Leader><Tab>", function() vim.cmd.edit("#") end)
nmap("zh", "20zh")
nmap("zl", "20zl")

-- Tabpages
nmap("<Leader>1", function() vim.cmd.tabnext(1) end)
nmap("<Leader>2", function() vim.cmd.tabnext(2) end)
nmap("<Leader>3", function() vim.cmd.tabnext(3) end)
nmap("<Leader>4", function() vim.cmd.tabnext(4) end)
nmap("<Leader>5", function() vim.cmd.tabnext(5) end)
nmap("<Leader>6", function() vim.cmd.tabnext(6) end)
nmap("<Leader>7", function() vim.cmd.tabnext(7) end)
nmap("<Leader>8", function() vim.cmd.tabnext(8) end)
nmap("<Leader>9", function() vim.cmd.tabnext(9) end)

nmap("<Leader>T", vim.cmd.tabnew)

-- Lsp
nmap("gh", vim.lsp.buf.hover, "Open LSP symbol hover information")
nmap("gH", vim.diagnostic.open_float, "Open LSP diagnostic")
nmap("gd", vim.lsp.buf.definition, "Go to Definition")
nmap("gD", vim.lsp.buf.type_definition, "Go to Type definition")
nmap("gi", vim.lsp.buf.implementation, "Go to Impementation")
nmap("gr", vim.lsp.buf.references, "Go to References")
nmap("ge", vim.lsp.buf.rename, "Edit symbol")
nmap("<Leader>cf", vim.lsp.buf.format, "Code Format")
nmap("<Leader>cd", vim.diagnostic.setloclist, "Code Diagnostics")
nmap("<Leader>ca", vim.lsp.buf.code_action, "Code Actions")
nmap("<Leader>sbs", vim.lsp.buf.document_symbol, "Search Buffer Symbols")
nmap("<Leader>ss", vim.lsp.buf.workspace_symbol, "Search workspace Symbols")

-----------------
--== Plugins ==--
-----------------

local function gh(uri)
    return "https://github.com/" .. uri
end

vim.pack.add({
    { src = gh("nvim-mini/mini.icons") }, -- Dependency for oil.nvim
    { src = gh("nvim-lua/plenary.nvim") },
    { src = gh("stevearc/oil.nvim") },
    { src = gh("neovim/nvim-lspconfig") },
    { src = gh("pmizio/typescript-tools.nvim") },
    { src = gh("nvim-treesitter/nvim-treesitter") },
    { src = gh("Decodetalkers/csharpls-extended-lsp.nvim") },
    { src = gh("j-hui/fidget.nvim") },
    { src = gh("nvim-mini/mini.pick") },
    { src = gh("saghen/blink.cmp"), version = "v1.8.0" },
    { src = gh("lewis6991/gitsigns.nvim") }
})

require("mini.icons").setup({})
require("fidget").setup({})
require("oil").setup({})

-- Treesitter
local treesitter = require("nvim-treesitter")
local treesitter_languages = { "vue", "javascript", "typescript", "c_sharp", "markdown", "markdown_inline", "json" }
treesitter.install(treesitter_languages)

vim.api.nvim_create_autocmd('FileType', {
    pattern = treesitter_languages,
    callback = function() vim.treesitter.start() end,
})

-------------
--== LSP ==--
-------------

--[[
typescript-tools with vue integration requires the following npm packages to be installed globally:
    - typescript
    - @vue/language-server
    - @vue/typescript-plugin
]]
require("typescript-tools").setup({
    filetypes = {
        "javascript",
        "typescript",
        "vue",
    },
    settings = {
        tsserver_plugins = {
            "@vue/typescript-plugin"
        }
    }
})

vim.lsp.inline_completion.enable(true)

local progress = require("fidget.progress")

vim.api.nvim_create_autocmd('LspRequest', {
    callback = (function()
        local pending_lsp_requests = {}
        return function(args)
            local request_id = args.data.request_id
            local request = args.data.request

            local allowed_methods = {
                ["textDocument/definition"] = true,
                ["textDocument/typeDefinition"] = true,
                ["textDocument/references"] = true,
                ["textDocument/implementation"] = true,
                ["textDocument/codeAction"] = true
            }

            if not allowed_methods[request.method] then
                return
            end

            if request.type == 'pending' then
                pending_lsp_requests[request_id] = progress.handle.create({
                    message = "Lsp action loading",
                    title = request.method,
                    lsp_client = vim.lsp.get_client_by_id(args.data.client_id)
                })
            elseif request.type == 'cancel' then
                pending_lsp_requests[request_id]:cancel()
            elseif request.type == 'complete' then
                pending_lsp_requests[request_id]:finish()
            end
        end
    end)(),
})

local mini_pick = require("mini.pick")
mini_pick.setup({})

nmap("<Leader>b", mini_pick.builtin.buffers)
nmap("<Leader><Leader>", mini_pick.builtin.files)
nmap("<Leader>sg", mini_pick.builtin.grep_live)
nmap("<Leader>sr", mini_pick.builtin.resume)

require("blink.cmp").setup({
    completion = {
        ghost_text = {
            enabled = true
        },
        menu = {
            auto_show = false
        }
    },
    keymap = {
        preset = "none",
        ["<C-n>"] = { "show", "select_next" },
        ["<C-p>"] = { "show", "select_prev" },
        ["<C-y>"] = { "accept" },
        ["<C-l>"] = { "show_documentation" },
    }
})

------------------------------
--== Custom functionality ==--
------------------------------

-- Rider integration
local rider_binary = "C:\\Program Files\\JetBrains\\JetBrains Rider 2025.3.1\\bin\\rider64.exe"

local function open_in_rider(file, line, column)
    if line == nil then
        line = 0
    end
    if column == nil then
        column = 0
    end
    vim.system({ rider_binary, "--line", line, "--column", column, file })
end

local function open_current_file_in_rider()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local row = cursor[1]
    local column = cursor[2]
    open_in_rider(vim.api.nvim_buf_get_name(0), row, column)
end

nmap("<Leader>or", open_current_file_in_rider)

-- Load vim lua library for lua_ls
vim.lsp.config("lua_ls", {
    settings = {
        Lua = {
            workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false
            }
        }
    }
})

vim.lsp.enable("lua_ls")

-- csharp_ls is installed with: `dotnet tool install --global csharp-ls`
vim.lsp.enable("csharp_ls")
require("csharpls_extended").buf_read_cmd_bind()

