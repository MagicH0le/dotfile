local loaded_lsp, lsp = xpcall(require, debug.traceback, "lspconfig")
if not loaded_lsp then
    vim.notify_once(string.format("There was an error requiring 'lspconfig'. Traceback:\n%s", lsp), vim.log.levels.ERROR)
end

local loaded_mason, mason = xpcall(require, debug.traceback, "mason")
if not loaded_mason then
    vim.notify_once(string.format("There was an error requiring 'mason'. Traceback:\n%s", mason), vim.log.levels.ERROR)
end

local loaded_mlsp, mlsp = xpcall(require, debug.traceback, "mason-lspconfig")
if not loaded_mlsp then
    vim.notify_once(string.format("There was an error requiring 'mason-lspconfig'. Traceback:\n%s", mlsp), vim.log.levels.ERROR)
end

mason.setup()

local function on_attach(_, bufnr)
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.MiniCompletion.completefunc_lsp')
end

mlsp_options = {
    ensure_installed = {
        "lua_ls"
    }
}
mlsp.setup(mlsp_options)
mlsp.setup_handlers({
    function(server)
        lsp[server].setup({
            on_attach = on_attach
        })
    end,
    ["lua_ls"] = function()
        lsp.lua_ls.setup({
            on_attach = on_attach,
            settings = {
                Lua = {
                    hint = {
                        enable = true
                    },
                    runtime = {
                        version = "LuaJIT"
                    },
                    diagnostics = {
                        globals = { "_G", "vim" },
                    },
                    workspace = {
                        preloadFileSize = 500
                    }
                }
            }
        })
    end
})
