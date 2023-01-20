-- Terminal autocmd
local disableNumbers = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
end

vim.api.nvim_create_autocmd({"TermOpen"}, {
    pattern = {"term://*"},
    callback = disableNumbers,
})
