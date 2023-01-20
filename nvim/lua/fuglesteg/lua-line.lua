-- FIXME: Fix status clock or remove
-- Lua line config
local function statusClock()
    local timeTable = os.date("*t")
    local timeHourMinute = timeTable.hour + ":" + timeTable.min
    return timeHourMinute
end
require('lualine').setup {
    options = { theme = 'tokyonight' },
    sections = { lualine_a = { statusClock } }
}
