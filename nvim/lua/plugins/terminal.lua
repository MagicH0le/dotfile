local loaded_terminal, terminal = xpcall(require, debug.traceback, "toggleterm")
if not loaded_terminal then
    vim.notify_once(string.format("There was an error requiring 'toggleterm'. Traceback:\n%s", terminal), vim.log.levels.ERROR)
end

local options = {
    direction = "horizontal"
}

terminal.setup(options)
