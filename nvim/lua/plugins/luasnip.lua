local loaded_luasnip, luasnip = xpcall(require, debug.traceback, "luasnip")
if not loaded_luasnip then
    vim.notify_once(string.format("There was an error requring 'luasnip'. Traceback:\n%s", luasnip), vim.log.levels.ERROR)
end

-- Load snippet from VSCode
require("luasnip.loaders.from_vscode").lazy_load()
require("luasnip.loaders.from_vscode").lazy_load({
    paths = vim.g.vscode_snippets_path or ""
})

-- Load snippet from SnipMate
require("luasnip.loaders.from_snipmate").lazy_load()
require("luasnip.loaders.from_snipmate").lazy_load({
    paths = vim.g.snipmate_snippets_path or ""
})

-- Load snippet from Lua
require("luasnip.loaders.from_lua").lazy_load()
require("luasnip.loaders.from_lua").lazy_load({
    paths = vim.g.lua_snippets_path or ""
})

vim.api.nvim_create_autocmd("InsertLeave", {
    callback = function()
        if luasnip.session.current_nodes[vim.api.nvim_get_current_buf()] and luasnip.session.jump_active then
            luasnip.unlink_current()
        end
    end
})
