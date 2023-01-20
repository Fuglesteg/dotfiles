-- Keybindings

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

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

-- nmap <silent><Esc> :nohl<cr>
nmap("<Esc>", ":FloatermHide<cr>:nohl<cr>")
nmap("<leader><leader>", ":Telescope find_files<cr>")
tmap("<C-a>", "<C-\\><C-n>")
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
nmap("<silent>J", "gT")
nmap("<silent>K", "gt")
-- nnoremap <C-v> +p
nmap("<silent>gd", ":lua vim.lsp.buf.definition()<cr>")
nmap("<silent>gr", ":Telescope lsp_references<cr>")

imap("<c-space>", "lua vim.lsp.buf.definition()<cr>")

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
        f = { ":Telescope file_browser<cr>", "Open File Browser" },
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
    C = { ":e ~/.config/nvim/init.lua<cr>", "Open config file" },
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
}, { prefix = "<leader>" })

