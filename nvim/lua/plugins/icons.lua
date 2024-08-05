local loaded_icons, icons = xpcall(require, debug.traceback, "mini.icons")
if not loaded_icons then
    vim.notify_once(string.format("There was an error requiring 'mini.icons'. Traceback:\n%s", icons), vim.log.levels.ERROR)
end

icons.setup()
