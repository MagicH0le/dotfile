local loaded_indentscope, indentscope = xpcall(require, debug.traceback, "mini.indentscope")
if not loaded_indentscope then
    vim.notify_once(string.format("There was an error requiring 'mini.indentscope'. Traceback:\n%s", indentscope), vim.log.levels.ERROR)
end

local options = {
    symbol = '|'
}

indentscope.setup(options)
