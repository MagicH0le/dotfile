local loaded_git, git = xpcall(require, debug.traceback, "mini.git")
if not loaded_git then
    vim.notify_once(string.format("There was an error requiring 'mini.git'. Traceback\n:%s", git), vim.log.levels.ERROR)
end

git.setup()
