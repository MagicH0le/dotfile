local loaded_files, files = xpcall(require, debug.traceback, "mini.files")
if not loaded_files then
    vim.notify_once(string.format("There was an error requiring 'mini.files'. Traceback\n:%s", files), vim.log.levels.ERROR)
end

files.setup()
