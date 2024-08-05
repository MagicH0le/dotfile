local loaded_tabline, tabline = xpcall(require, debug.traceback, "mini.tabline")
if not loaded_tabline then
    vim.notify_once(string.format("There was an error requiring 'mini.tabline'. Traceback:\n%s", tabline), vim.log.levels.ERROR)
end

tabline.setup()
