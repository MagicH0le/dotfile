local loaded_comp, comp = xpcall(require, debug.traceback, "mini.completion")
if not loaded_comp then
    vim.notify_once(string.format("There was an error requiring 'mini.completion'. Traceback:\n%s", comp), vim.log.levels.ERROR)
end

local options = {
    lsp_completion = {
        auto_setup = false,
        source_func = "omnifunc"
    }
}

comp.setup(options)
