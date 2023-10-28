-- Terminal autocmd
local disableNumbers = function()
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
end

vim.api.nvim_create_autocmd({"TermOpen"}, {
    pattern = {"term://*"},
    callback = disableNumbers,
})

local function getSelectionRange()
    local _, srow, scol, _ = unpack(vim.fn.getpos("v"))
    local _, erow, ecol, _ = unpack(vim.fn.getpos("."))
    -- return srow, scol, erow, ecol
    if srow < erow or (srow == erow and scol <= ecol) then
        return srow - 1, scol - 1, erow, ecol
    else
        return erow, ecol - 1, srow - 1, scol
    end
end

local function getSelection()
    local srow, scol, erow, ecol = getSelectionRange()
    return vim.api.nvim_buf_get_text(0, srow, scol, erow, ecol, {})
end

local T = {}

T.setTermAsMain = function()
    local job_id = vim.b.terminal_job_id
    T.job_id = job_id
end

T.send = function(data)
    data = table.concat(data, "\r")
    vim.api.nvim_chan_send(T.job_id, data)
end

T.execute = function(data)
    data[#data] = "\r"
    T.send(data)
end

T.executeSelection = function()
    T.execute(getSelection())
end

return T
