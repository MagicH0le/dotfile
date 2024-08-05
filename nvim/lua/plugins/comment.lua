local loaded_comment, comment = xpcall(require, debug.traceback, "mini.comment")
if not loaded_comment then
    vim.notify_once(string.format("There was an error requiring 'mini.comment'. Traceback\n:%s", comment), vim.log.levels.ERROR)
end

comment.setup()
