-- Keybindings


-- Utility functions

local function remap(mode, binding, command)
    vim.keymap.set(mode, binding, command, { silent = true })
end

local function nmap(binding, command)
    remap('n', binding, command)
end

local function vmap(binding, command)
    remap('v', binding, command)
end

local function imap(binding, command)
    remap('i', binding, command)
end

local function tmap(binding, command)
    remap('t', binding, command)
end

nmap("j", "gj")
nmap("k", "gk")

-- nmap <silent><Esc> :nohl<cr>
nmap("<Esc>", ":FloatermHide<cr>:nohl<cr>")
nmap("<leader><leader>", ":Telescope find_files<cr>")
tmap("<Esc>", "<C-\\><C-n>")
-- inoremap <C-j> <Down>
-- inoremap <C-k> <Up>
-- inoremap <C-h> <Left>
-- inoremap <C-l> <Right>
nmap("<C-j>", "<Tab>")
nmap("<C-k>", "<S-Tab>")
nmap("<leader>;", ":Commentary<cr>")
vmap("<leader>;", ":Commentary<cr>")
vmap(">", ">gv")
vmap("<", "<gv")
-- nmap("J", "gT")
-- nmap("K", "gt")
-- nnoremap <C-v> +p
nmap("gd", ":lua vim.lsp.buf.definition()<cr>")
nmap("gr", ":Telescope lsp_references<cr>")
nmap("gh", vim.lsp.buf.hover)

-- imap("<c-space>", "lua vim.lsp.buf.definition()<cr>")

nmap("<F1>", require("dap").continue, "Start or continue debug session")
nmap("<F2>", require("dap").step_over, "Step over")
nmap("<F3>", require("dap").step_into, "Step into")

local possession_session = require("possession.session")
local function saveSessionPrompt()
    local session_name = possession_session.session_name or ""
    session_name = vim.fn.input("Session: ", session_name)
    if session_name ~= "" then
        possession_session.save(session_name)
    end
end

local possession = require("possession")
local function renameSessionPrompt()
    local session_name = possession_session.session_name or ""
    if session_name == "" then
        print("Error: Not currently in a session")
        return
    end
    local new_session_name = vim.fn.input("New session name: ", session_name)
    if new_session_name == "" then
        print("Error: No name provided")
        return
    end
    possession.save(new_session_name)
    possession.delete(session_name, { no_confirm = true })
end

local function loadLastSession()
    local last_session_path = possession.last()
    if last_session_path == nil then
        print("Could not find previous session")
        return
    end
    local last_session_file = string.gsub(last_session_path, ".*/", "")
    local last_session_name = string.gsub(last_session_file, ".json", "")
    possession.load(last_session_name)
end

-- Which-key config
local wk = require("which-key")
wk.register({
    f = {
        name = "+File",
        s = { ":w<cr>", "Save file" },
        S = { ":wall<cr>", "Save all files" },
        q = { ":wq<cr>", "Write and quit file" },
        Q = { ":wqall<cr>", "Write and quit all files" },
        t = { ":NvimTreeToggle<cr>", "Open NvimTree" },
        p = { ":Telescope file_browser<cr>", "Open File Browser in cwd" },
        f = { ":Telescope file_browser path=%:p:h select_buffer=true<cr>", "Open File Browser in directory of open file" },
        r = { ":Telescope oldfiles<cr>", "Open recent files" },
        --! = { ":qall!<cr>", "Quit all files" },
    },
    w = {
        name = "+Window",
        v = { ":vs<cr>", "Vertical Split" },
        s = { ":split<cr>", "Horizontal Split" },
        l = { "<C-w>l", "Go left" },
        h = { "<C-w>h", "Go right" },
        j = { "<C-w>j", "Go down" },
        k = { "<C-w>k", "Go up" },
        d = { "<C-w>c", "Close" },
    },
    b = {
        name = "+Buffer",
    },
    c = {
        name = "+Code",
        s = { ":Telescope live_grep<cr>", "Fuzzy find" },
        a = { ":lua vim.lsp.buf.code_action()<cr>", "Actions" },
        e = { ":lua vim.lsp.buf.rename()<cr>", "Rename" },
        d = { ":Telescope lsp_definitions<cr>", "Go to definition" },
        r = { ":Telescope lsp_references<cr>", "Go to references" },
        f = { ":Telescope lsp_document_symbols<cr>", "Find symbols in file" },
    },
    g = {
        name = "+Git",
        s = { ":Neogit<cr>", "git status" },
        d = {
            name = "+Diff",
        }
    },
    s = {
        name = "+Search",
        s = { ":Telescope possession list<cr>", "Sessions" },
        b = { ":Telescope buffers<cr>", "Buffers" },
        g = { ":Telescope live_grep<cr>", "Grep" },
        c = { ":Telescope current_buffer_fuzzy_find<cr>", "Fuzzy current file" },
        f = { ":Telescope files<cr>", "Grep" },
        r = { ":Telescope resume<cr>", "Resume last search" },
        o = { ":Telescope oldfiles<cr>", "Oldfiles" },
        q = { ":Telescope quickfix<cr>", "Quickfix" },
        m = { ":Telescope marks<cr>", "Marks" },
    },
    S = {
        name = "+Session",
        l = { ":Telescope possession list<cr>", "List sessions" },
        s = { saveSessionPrompt, "Save session"},
        r = { renameSessionPrompt, "Rename session"},
        t = { ":PossessionLoad tmp<cr>", "Restore temp session"},
        o = { loadLastSession, "Open last session"},
        d = { ":PossessionDelete ", "Delete session"},
        c = { ":PossessionClose<cr>", "Close Session"},
    },
    C = {
        name = "+Configure",
        c = {":e ~/.config/nvim/init.lua | cd ~/.config/nvim<cr>", "Open config file" },
        -- Add quick options hydra
    },
    t = {
        name = "+Terminal",
        t = { ":FloatermToggle<cr>", "Toggle floating terminal" },
        h = { ":FloatermNext<cr>", "Next floating terminal" },
        l = { ":FloatermPrev<cr>", "Previous floating terminal" },
        d = { ":FloatermKill<cr>", "Kill floating terminal" },
        n = { ":FloatermNew<cr>", "New floating terminal" },
        b = { ":term<cr>", "Open new terminal in buffer" },
    },
    p = {
        name = "+Projects",
        p = { ":Telescope projects<cr>", "Recent projects" }
    },
    T = { vim.cmd.tabnew, "New tab" },
    ["1"] = { function() vim.api.nvim_set_current_tabpage(1) end, "Tab 1"},
    ["2"] = { function() vim.api.nvim_set_current_tabpage(2) end, "Tab 2"},
    ["3"] = { function() vim.api.nvim_set_current_tabpage(3) end, "Tab 3"},
    ["4"] = { function() vim.api.nvim_set_current_tabpage(4) end, "Tab 4"},
    ["5"] = { function() vim.api.nvim_set_current_tabpage(5) end, "Tab 5"},
    ["6"] = { function() vim.api.nvim_set_current_tabpage(6) end, "Tab 6"},
    ["7"] = { function() vim.api.nvim_set_current_tabpage(7) end, "Tab 7"},
    ["8"] = { function() vim.api.nvim_set_current_tabpage(8) end, "Tab 8"},
    ["9"] = { function() vim.api.nvim_set_current_tabpage(9) end, "Tab 9"},
    z = { ":TZAtaraxis<cr>", "Zen" },
    ["<tab>"] = { "<c-6>", "Switch buffer"},
}, { prefix = "<leader>" })

