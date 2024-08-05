local loaded_diff, diff = xpcall(require, debug.traceback, "mini.diff")
if not loaded_diff then
    vim.notify_once(string.format("There was an error requiring 'mini.diff'. Traceback\n:%s", diff), vim.log.levels.ERROR)
end

diff.setup()
