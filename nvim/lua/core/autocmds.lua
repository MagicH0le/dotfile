local autocmd = vim.api.nvim_create_autocmd

-- Create directory on save file when directory not exist
autocmd("BufWritePre", {
    pattern = "*",
    callback = function()
        require("utils.autocmds").create_directory_on_save()
    end
})

-- Quick close help, notify, health check, etc.
autocmd("FileType", {
    pattern = { "help", "notify", "checkhealth" },
    callback = function()
        vim.keymap.set("n", "q", "<cmd>close<CR>", { silent = true, buffer = true })
    end
})

-- Delete trailing whitespace
autocmd("BufWritePre", {
    pattern = "*",
    command = ":%s/\\s\\+$//e"
})

-- Don't set conceal in json
autocmd("FileType", {
    pattern = { "json", "jsonc", "json5" },
    callback = function()
        vim.opt_local.conceallevel = 0
    end
})

-- Don't add comment line after a comment line
autocmd("BufEnter", {
    pattern = "",
    command = "set fo-=c fo-=r fo-=o"
})

-- Format on save
-- autocmd("BufWritePre", {
--     pattern = "",
--     command = ":silent lua vim.lsp.buf.format()"
-- })
