local loaded_core, core_err = xpcall(require, debug.traceback, "core")
if not loaded_core then
    vim.notify_once(string.format("There was an error requiring 'core'. Traceback:\n%s", core_err), vim.log.levels.ERROR)
end
