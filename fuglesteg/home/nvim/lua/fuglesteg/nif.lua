local T = {}

T.openTranslationTabpage = function ()
    local enTranslationFile = vim.fn.findfile("en.json", "**")
    local noTranslationFile = vim.fn.findfile("no.json", "**")

    if enTranslationFile == "" or noTranslationFile == "" then
        vim.notify("Could not find translation files", vim.log.levels.ERROR)
        return
    end

    vim.cmd.tabnew({enTranslationFile})
    vim.cmd.diffthis()
    vim.cmd.vsplit({noTranslationFile})
    vim.cmd.diffthis()
end

return T
