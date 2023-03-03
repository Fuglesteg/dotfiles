return {
    "iamcco/markdown-preview.nvim",
    build = "cd app && npm install",
    ft = { "markdown" },
    config = function()
        vim.g.mkdp_filetypes = { "markdown" }
        vim.g.mkdp_open_to_the_world = 1
        vim.g.mkdp_open_ip = "127.0.0.1"
        vim.g.mkdp_port = 8080
        vim.cmd [[
function! g:EchoUrl(url)
    :echo a:url
endfunction
]]
        vim.g.mkdp_browserfunc = "g:EchoUrl"
    end
}
