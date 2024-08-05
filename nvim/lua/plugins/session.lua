local loaded_sessions, sessions = xpcall(require, debug.traceback, "mini.sessions")
if not loaded_sessions then
    vim.notify_once(string.format("There was an error requiring 'mini.sessions'. Traceback:\n%s", sessions), vim.log.levels.ERROR)
end

local options = {
    directory = vim.fn.stdpath("data") .. "/session"
}

sessions.setup(options)
