local loaded_pairs, pairs = xpcall(require, debug.traceback, "mini.pairs")
if not loaded_pairs then
    vim.notify_once(string.format("There was an error requiring 'mini.pairs'. Traceback:\n%s", pairs), vim.log.levels.ERROR)
end

local options = {
    modes = {
        insert = true,
        command = true,
        terminal = false
    }
}

pairs.setup(options)
