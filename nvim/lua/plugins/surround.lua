local loaded_surround, surround = xpcall(require, debug.traceback, "mini.surround")
if not loaded_surround then
    vim.notify_once(string.format("There was an error requiring 'mini.surround'. Traceback:\n%s", surround), vim.log.levels.ERROR)
end

surround.setup()
