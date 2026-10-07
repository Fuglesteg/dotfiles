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
vim.api.nvim_set_hl(0, "NeogitDiffAddHighlight", { fg = "#4e904e" })
vim.api.nvim_set_hl(0, "NeogitDiffAdd", { fg = "#4e904e" })
vim.api.nvim_set_hl(0, "NeogitDiffDelete", { bg = "#994b3c" })
vim.api.nvim_set_hl(0, "NeogitDiffDeleteCursor", { fg = "#93979e" })
vim.api.nvim_set_hl(0, "NeogitDiffDeletions", { fg = "#93979e" })
vim.api.nvim_set_hl(0, "NeogitDiffDeleteHighlight", { fg = "#93979e" })
vim.api.nvim_set_hl(0, "NeogitDeletions", { fg = "#93979e" })
vim.api.nvim_set_hl(0, "NeogitDeleteCursor", { fg = "#93979e" })
vim.api.nvim_set_hl(0, "@lsp.type.parameter", { fg="#ded266" })
vim.api.nvim_set_hl(0, "@lsp.mod.readonly", { italic=true })


-- Ignore files for vimgrep, etc.
vim.g.wildignore = "bin/**,node_modules/**,*.o,*.obj,*.dll,*.svg,*.png,*.jpg"

vim.g.mapleader = ' '
vim.g.maplocalleader = ','

local function nmap(binding, action, description)
    vim.keymap.set('n', binding, action, { desc = description })
end

local function vmap(binding, action, description)
    vim.keymap.set('v', binding, action, { desc = description })
end

local function tmap(binding, action, description)
    vim.keymap.set('t', binding, action, { desc = description })
end

local function lmap(binding, action, description)
    nmap("<Leader>" .. binding, action, description)
end

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
    { src = gh("j-hui/fidget.nvim") },
    { src = gh("saghen/blink.cmp"), version = "v1.8.0" },
    { src = gh("lewis6991/gitsigns.nvim") },
    { src = gh("NeogitOrg/neogit") },
    { src = gh("folke/which-key.nvim") },
    { src = gh("folke/snacks.nvim") },
    { src = gh("olimorris/codecompanion.nvim") },
    { src = gh("OXY2DEV/markview.nvim") },
    { src = gh("nvim-mini/mini.icons") },
    { src = gh("sindrets/diffview.nvim") },
})

require("snacks").setup({
    picker = {
        ui_select = true,
        layout = {
            preset = "default"
        }
    }
})

require("markview").setup({
    preview = {
        filetypes = { "codecompanion", "markdown" },
        ignore_buftypes = { "nofile" },
        condition = function(buffer)
            return vim.bo[buffer].filetype == "codecompanion" or nil
        end
    }
})

--[[
Requires the `claude-agent-acp` binary on PATH (npm package
@agentclientprotocol/claude-agent-acp), which bridges CodeCompanion's ACP
client to the Claude Agent SDK. It reuses the same login as the `claude` CLI,
so no separate API key/token is needed once `claude` is authenticated.
]]
require("codecompanion").setup({
    display = {
        chat = {
            window = {
                layout = "float"
            }
        }
    },
    interactions = {
        chat = {
            adapter = {
                name = "claude_code",
                model = "opus",
            },
            opts = {
                completion_provider = "blink",
                ---@param ctx CodeCompanion.SystemPrompt.Context
                ---@return string
                system_prompt = function(ctx)
                    return ctx.default_system_prompt ..
                    [[Additional context:
                    Give full pathnames and line numbers when referencing files so that vim `gf` works to jump to that location.
                    Make sure that code block start and end backquotes '````' are on their own lines.
                    ]]
                end,
            }
        },
        shared = {
            keymaps = {
                always_accept = {
                    callback = "keymaps.always_accept",
                    modes = { n = "gA" },
                },
                accept_change = {
                    callback = "keymaps.accept_change",
                    modes = { n = "ga" },
                },
                reject_change = {
                    callback = "keymaps.reject_change",
                    modes = { n = "gr" },
                },
                next_hunk = {
                    callback = "keymaps.next_hunk",
                    modes = { n = "}" },
                },
                previous_hunk = {
                    callback = "keymaps.previous_hunk",
                    modes = { n = "{" },
                },
            },
        },
    },
    adapters = {
        acp = {
            claude_code = function()
                return require("codecompanion.adapters").extend("claude_code", {
                    defaults = {
                        session_config_options = {
                            mode = "auto",
                            thought_level = "high",
                        }
                    }
                    --[[
                    env = {
                        CLAUDE_CODE_OAUTH_TOKEN = "my-oauth-token",
                    },
                    ]]
                })
            end,
        }
    }
})

lmap("ac", "<Cmd>CodeCompanionChat Toggle<CR>", "Toggle CodeCompanion Chat")
lmap("aC", "<Cmd>CodeCompanionChat<CR>", "CodeCompanion New Chat")
vmap("<Leader>ac", "<Cmd>CodeCompanionChat Add<CR>", "Add selection to CodeCompanion Chat")
lmap("ax", "<Cmd>CodeCompanionActions<CR>", "CodeCompanion Actions")
lmap("arq", "<Cmd>CodeCompanionCodeReview<CR>", "CodeCompanion Review Fill quickfix")
lmap("arc", "<Cmd>CodeCompanionCodeReview Comment<CR>", "CodeCompanion Review Comment")
vmap("arc", "<Cmd>CodeCompanionCodeReview Comment<CR>", "CodeCompanion Review Comment")
lmap("ara", "<Cmd>CodeCompanionCodeReview Approve<CR>", "CodeCompanion Review Approve")
lmap("arr", "<Cmd>CodeCompanionCodeReview Reject<CR>", "CodeCompanion Review Reject")
lmap("ars", "<Cmd>CodeCompanionCodeReview Share<CR>", "CodeCompanion Review Share")

require("mini.icons").setup({})
require("fidget").setup({})
require("oil").setup({})
require("which-key").setup({
    delay = 2000
})

-- Git
local gitsigns = require("gitsigns")
gitsigns.setup({
    attach_to_untracked = false,
})

lmap("gsb", gitsigns.stage_buffer, "Stage buffer")
lmap("gsh", gitsigns.stage_hunk, "Stage hunk")
lmap("gd", gitsigns.diffthis, "Diff this buffer")
lmap("gbb", gitsigns.blame, "Blame buffer")
lmap("gbl", gitsigns.blame_line, "Blame line")
lmap("gq", function() gitsigns.setqflist("all") end, "Send all hunks to quickfix list")
lmap("gl", gitsigns.setloclist, "Send hunks to location list")
lmap("grh", gitsigns.reset_hunk, "Reset hunk")
lmap("gn", function() gitsigns.nav_hunk("next") end, "Go to next hunk")
lmap("gp", function() gitsigns.nav_hunk("prev") end, "Go to previous hunk")
lmap("gP", gitsigns.preview_hunk_inline, "Preview hunk inline")

local neogit = require("neogit")

lmap("gg", neogit.open, "Open Neogit")

local treesitter_languages = {
    "vue",
    "javascript",
    "typescript",
    "jsdoc",
    "c_sharp",
    "markdown",
    "markdown_inline",
    "json"
}

vim.api.nvim_create_autocmd('FileType', {
    pattern = treesitter_languages,
    callback = function() vim.treesitter.start() end,
})

------------------
--== Keybinds ==--
------------------
-- General
nmap("<Esc>", vim.cmd.nohlsearch, "Clear search highlight")
vmap("<", "<gv", "Indent left and reselect")
vmap(">", ">gv", "Indent right and reselect")
nmap("zh", "20zh", "Scroll 20 columns left")
nmap("zl", "20zl", "Scroll 20 columns right")
lmap("ff", vim.cmd.Oil, "Open file explorer")
lmap("<Tab>", function() vim.cmd.edit("#") end, "Edit alternate file")
tmap("<Esc><Esc>", "<C-\\><C-n>", "Exit terminal mode")

-- Tabpages
lmap("1", function() vim.cmd.tabnext(1) end, "Go to tab 1")
lmap("2", function() vim.cmd.tabnext(2) end, "Go to tab 2")
lmap("3", function() vim.cmd.tabnext(3) end, "Go to tab 3")
lmap("4", function() vim.cmd.tabnext(4) end, "Go to tab 4")
lmap("5", function() vim.cmd.tabnext(5) end, "Go to tab 5")
lmap("6", function() vim.cmd.tabnext(6) end, "Go to tab 6")
lmap("7", function() vim.cmd.tabnext(7) end, "Go to tab 7")
lmap("8", function() vim.cmd.tabnext(8) end, "Go to tab 8")
lmap("9", function() vim.cmd.tabnext(9) end, "Go to tab 9")

lmap("T", vim.cmd.tabnew, "Open new tab")

-- Lsp
nmap("gh", vim.lsp.buf.hover, "Open LSP symbol hover information")
nmap("gH", vim.diagnostic.open_float, "Open LSP diagnostic")
nmap("gd", Snacks.picker.lsp_definitions, "Go to Definition")
nmap("gD", Snacks.picker.lsp_type_definitions, "Go to Type definition")
nmap("gi", Snacks.picker.lsp_implementations, "Go to Impementation")
nmap("gr", Snacks.picker.lsp_references, "Go to References")
nmap("ge", vim.lsp.buf.rename, "Edit symbol")

lmap("cf", vim.lsp.buf.format, "Code Format")
lmap("cd", Snacks.picker.diagnostics, "Code Diagnostics")
lmap("ca", vim.lsp.buf.code_action, "Code Actions")
lmap("cs", Snacks.picker.lsp_symbols, "Code Symbols")
lmap("sbs", vim.lsp.buf.document_symbol, "Search Buffer Symbols")
lmap("ss", vim.lsp.buf.workspace_symbol, "Search workspace Symbols")

-------------
--== LSP ==--
-------------

vim.diagnostic.config({
	virtual_text = {
        -- Only show text for warning and error
		severity = { min = vim.diagnostic.severity.WARN },
	},
	signs = {
		text = {
			[vim.diagnostic.severity.ERROR] = "",
			[vim.diagnostic.severity.WARN] = "",
			[vim.diagnostic.severity.INFO] = "",
			[vim.diagnostic.severity.HINT] = "",
		},
	},
})

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

nmap("<Leader>b", Snacks.picker.buffers, "Search buffers")
nmap("<Leader><Leader>", Snacks.picker.files, "Find files")
nmap("<Leader>sg", Snacks.picker.grep, "Live grep")
nmap("<Leader>sr", Snacks.picker.resume, "Resume last search")
nmap("<Leader>so", Snacks.picker.recent, "Search old files")
nmap("<Leader>sb", Snacks.picker.lines, "Search buffer")

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

nmap("<Leader>or", open_current_file_in_rider, "Open current file in Rider")

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

--[[
-- Using roslyn_ls instead
vim.lsp.config("csharp_ls", {
    cmd = function(dispatchers, config)
        return vim.lsp.rpc.start({ "csharp-ls", "--features", "metadata-urls" }, dispatchers, {
            cwd = config.cmd_cwd or config.root_dir,
            env = config.cmd_env,
            detached = config.detached,
        })
    end
})
]]

-- roslyn_ls is installed with: `dotnet tool install --global --prerelease roslyn-language-server`

--=============================
-- Yoinked from nvim-lspconfig
--=============================

---@param client vim.lsp.Client
---@param target string
local function on_init_sln(client, target)
  vim.notify('Initializing: ' .. target, vim.log.levels.TRACE, { title = 'roslyn_ls' })
  ---@diagnostic disable-next-line: param-type-mismatch
  client:notify('solution/open', {
    solution = vim.uri_from_fname(target),
  })
end

---@param client vim.lsp.Client
---@param project_files string[]
local function on_init_project(client, project_files)
  vim.notify('Initializing: projects', vim.log.levels.TRACE, { title = 'roslyn_ls' })
  ---@diagnostic disable-next-line: param-type-mismatch
  client:notify('project/open', {
    projects = vim.tbl_map(function(file)
      return vim.uri_from_fname(file)
    end, project_files),
  })
end

--[[
On Linux the roslyn-language-server native apphost has its own broken
.NET-runtime discovery under Guix: it ignores a correctly-set DOTNET_ROOT
and instead recomputes it from wherever `dotnet` resolves on PATH, landing
on that store path's `bin/` dir instead of its `share/dotnet` dir (Guix's
dotnet package keeps them as siblings, not nested) - it then fails to find
the runtime at all. Bypassing the apphost and running the managed DLL
through `dotnet exec` avoids this, since the dotnet muxer resolves the
runtime via DOTNET_ROOT correctly.
]]
local roslyn_cmd
if vim.fn.has("win32") == 1 then
    roslyn_cmd = { "roslyn-language-server.cmd", "--stdio" }
else
    local exe_path = vim.fn.exepath("roslyn-language-server")
    local real_path = exe_path ~= "" and (vim.uv.fs_realpath(exe_path) or exe_path)
    local dll = real_path and vim.fs.joinpath(vim.fs.dirname(real_path), "Microsoft.CodeAnalysis.LanguageServer.dll")
    if dll and vim.uv.fs_stat(dll) then
        roslyn_cmd = { "dotnet", dll, "--stdio" }
    else
        roslyn_cmd = { "roslyn-language-server", "--stdio" }
    end
end

vim.lsp.config("roslyn_ls", {
    cmd = roslyn_cmd,
    settings = {
        -- better performance
        -- Without this, roslyn analyzes the whole project
        ["csharp|background_analysis"] = {
            dotnet_analyzer_diagnostics_scope = "openFiles",
            dotnet_compiler_diagnostics_scope = "openFiles",
        }
    },
    on_init = {
        function(client)
            local root_dir = client.config.root_dir

            -- Load the right omnium solution
            for entry, type in vim.fs.dir(root_dir) do
                if type == 'file' and entry == "Omnium.sln" then
                    on_init_sln(client, vim.fs.joinpath(root_dir, entry))
                    return
                end
            end

            -- try load first solution we find
            for entry, type in vim.fs.dir(root_dir) do
                if type == 'file' and (vim.endswith(entry, '.sln') or vim.endswith(entry, '.slnx')) then
                    on_init_sln(client, vim.fs.joinpath(root_dir, entry))
                    return
                end
            end

            -- if no solution is found load project
            for entry, type in vim.fs.dir(root_dir) do
                if type == 'file' and vim.endswith(entry, '.csproj') then
                    on_init_project(client, { vim.fs.joinpath(root_dir, entry) })
                end
            end
        end,
    },
})

vim.lsp.enable("roslyn_ls")

