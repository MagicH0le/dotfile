local loaded_statusline, statusline = xpcall(require, debug.traceback, "mini.statusline")
if not loaded_statusline then
    vim.notify_once(string.format("There was an error requiring 'mini.statusline'. Traceback:\n%s", statusline), vim.log.levels.ERROR)
end

statusline.setup()
