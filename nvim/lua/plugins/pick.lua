local loaded_pick, pick = xpcall(require, debug.traceback, "mini.pick")
if not loaded_pick then
    vim.notify_once(string.format("There was an error requiring 'mini.pick'. Traceback\n:%s", pick), vim.log.levels.ERROR)
end

pick.setup()
