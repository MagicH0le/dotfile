local command = vim.api.nvim_create_user_command
command("SudoWrite", function()
    require("utils.sudo").write()
end, { desc = "Write file with sudo permissions" })
