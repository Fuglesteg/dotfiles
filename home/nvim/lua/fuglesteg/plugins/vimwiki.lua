return {
    "vimwiki/vimwiki",
    config = function()
        vim.g.vimwiki_list = {
            {
                path = "/home/andy/Drive/",
                syntax = "markdown",
                ext = ".md"
            }
        }
        vim.g.vimwiki_global_ext = 0
    end
}
