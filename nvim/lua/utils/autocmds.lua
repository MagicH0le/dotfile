local M = {}

function M.preserve_position()
    if vim.fn.line("'\"") > 1 and vim.fn.line("'\"") <= vim.fn.line("$") then
        vim.cmd("normal! g'\"")
    end
end

function M.create_directory_on_save()
    local fpath = vim.fn.expand("<afile>")
    local dir = vim.fn.fnamemodify(fpath, ":p:h")

    if vim.fn.isdirectory(dir) ~= 1 then
        vim.fn.mkdir(dir, "p")
    end
end

return M
