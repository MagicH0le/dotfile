local keymap = vim.keymap.set
vim.g.mapleader = " "

-- Switch window
keymap("n", "<leader>h", "<C-w>h")
keymap("n", "<leader>j", "<C-w>j")
keymap("n", "<leader>k", "<C-w>k")
keymap("n", "<leader>l", "<C-w>l")

-- Buffer
keymap("n", "<leader>bj", "<cmd>bnext<CR>")
keymap("n", "<leader>bk", "<cmd>bprevious<CR>")
keymap("n", "<leader>bx", "<cmd>bdelete<CR>")

-- Write
keymap("n", "<leader>w", "<cmd>write<CR>")
keymap("n", "<leader>W", "<cmd>write!<CR>")

-- Quit
keymap("n", "<leader>q", "<cmd>quit<CR>")
keymap("n", "<leader>Q", "<cmd>quit!<CR>")

-- Split window
keymap("n", "<leader>wh", "<cmd>split<CR>")
keymap("n", "<leader>wv", "<cmd>vsplit<CR>")

-- Terminal
keymap("n", "<Esc>", "<C-\\><C-n>")
