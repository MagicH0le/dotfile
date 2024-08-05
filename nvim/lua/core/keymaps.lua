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

-- File tree
keymap("n", "<leader>ft", "<cmd>lua MiniFiles.open()<CR>")

-- Fuzzy
keymap("n", "<leader>ff", "<cmd>lua MiniPick.builtin.files()<CR>")
keymap("n", "<leader>fg", "<cmd>lua MiniPick.builtin.grep_live()<CR>")
keymap("n", "<leader>fh", "<cmd>lua MiniPick.builtin.help()<CR>")
keymap("n", "<leader>fb", "<cmd>lua MiniPick.builtin.buffers()<CR>")

-- Terminal
keymap("n", "<leader>t", "<cmd>ToggleTerm<CR>")
