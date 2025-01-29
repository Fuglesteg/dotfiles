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

function endsWith(string, suffix)
    return string:sub(-#suffix) == suffix
end

T.testsSwitch = function()
    local fileName = vim.fs.basename(vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf()))
    local isTestFile = endsWith(fileName, "Tests.cs")
    if isTestFile then
        T.fileImplementationOpen(fileName)
    else
        T.fileTestsOpen(fileName)
    end
end

T.fileImplementationOpen = function(fileName)
    local implementationFileName = fileName:sub(0, #fileName - #"Tests.cs") .. ".cs"
    local implementationFile = vim.fn.findfile(implementationFileName, "**")
    if implementationFile ~= "" then
        vim.cmd.edit(implementationFile)
    else
        vim.notify("Could not find implementation file: " .. implementationFileName, vim.log.levels.ERROR)
    end
end

T.fileTestsOpen = function(fileName)
    local testFileName = fileName:sub(0, #fileName - 3) .. "Tests.cs"
    local testFile = vim.fn.findfile(testFileName, "**")
    if testFile ~= "" then
        vim.cmd.edit(testFile)
    else
        vim.notify("Could not find test file: " .. testFileName, vim.log.levels.ERROR)
    end
end

return T
