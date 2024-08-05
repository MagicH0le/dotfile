local loaded_notify, notify = xpcall(require, debug.traceback, "mini.notify")
if not loaded_notify then
    vim.notify_once(string.format("There was an error requiring 'mini.notify'. Traceback:\n%s", notify), vim.log.levels.ERROR)
end

local options = {
    window = {
        winblend = 0,
        config = { border = "rounded" }
    }
}
local notify_options = {
    ERROR = { duration = 5000 },
    WARN = { duration = 5000 },
    INFO = { duration = 3000 }
}

notify.setup(options)
vim.notify = notify.make_notify(notify_options)
