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

-- imap("<c-space>", "lua vim.lsp.buf.definition()<cr>")

nmap("<F1>", require("dap").continue, "Start or continue debug session")
nmap("<F2>", require("dap").step_over, "Step over")
nmap("<F3>", require("dap").step_into, "Step into")

local possession = require("possession.session")
local function promptForSessionName()
    local session_name = possession.session_name or ""
    session_name = vim.fn.input("Session: ", session_name)
    if session_name ~= "" then
        possession.save(session_name)
    end
end
local function deleteSession(session_name)
    possession.delete(session_name)
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
        b = { ":Telescope buffers<cr>", "Buffers" }
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
    },
    S = {
        name = "+Session",
        l = { ":Telescope possession list<cr>", "List sessions" },
        s = { promptForSessionName, "Save session"},
        r = { ":PossessionLoad tmp<cr>", "Restore last session"},
        d = { ":PossessionDelete ", "Delete session", silent = false},
        c = { ":PossessionClose<cr>", "Close Session"},
    },
    C = {
        name = "+Configure",
        c = {":e ~/.config/nvim/init.lua | cd ~/.config/nvim<cr>", "Open config file" },
        r = {":source ~/.config/nvim/init.lua<cr>", "Reload config" },
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
    h = { ":lua vim.lsp.buf.hover()<cr>", "View documentation" },
    ["<tab>"] = { "<c-6>", "Switch buffer"},
    -- d = {
    --     name = "+Debug",
    --     b = { require("dap").toggle_breakpoint, "Toggle breakpoint" },
    --     s = { require("dap").continue, "Start or continue debug session" },
    --     o = { require("dap").step_over, "Step over" },
    --     i = { require("dap").step_into, "Step into" },
    -- }
}, { prefix = "<leader>" })

