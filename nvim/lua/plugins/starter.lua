local loaded_starter, starter = xpcall(require, debug.traceback, "mini.starter")
if not loaded_starter then
    vim.notify_once(string.format("There was an error requiring 'mini.starter'. Traceback\n:%s", starter), vim.log.levels.ERROR)
end

starter.setup()
