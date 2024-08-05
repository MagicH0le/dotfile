local opt = vim.opt
local system = vim.uv.os_uname().sysname

-- Multi buffer
opt.hidden = true

-- Virtual editing
opt.virtualedit = "block"

-- Swap, undo, etc.
opt.swapfile = false
opt.undofile = true
opt.undodir = vim.fn.stdpath("state") .. "/undo"

-- History
opt.history = 1000

-- Performance
opt.timeout = true
opt.ttimeout = true
opt.timeoutlen = 500
opt.ttimeoutlen = 10
opt.updatetime = 100
opt.redrawtime = 1500

-- Matching
opt.magic = true
opt.smartcase = true
opt.ignorecase = true
opt.wildignorecase = true

-- Smooth scroll
opt.smoothscroll = true

-- Line number
opt.number = true
opt.relativenumber = true

-- True color
opt.termguicolors = true

-- Cursor line
opt.cursorline = true

-- Status line
opt.ruler = false
opt.showmode = false
opt.showcmd = false
opt.cmdheight = 0

-- Line wrap
opt.wrap = false

-- Indent
opt.copyindent = true
opt.smartindent = true
opt.preserveindent = true
opt.breakindent = true

-- Tab
opt.tabstop = 4
opt.softtabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.smarttab = true

-- Conceal
opt.conceallevel = 2

-- Split window
opt.splitright = true
opt.splitbelow = true

-- Clipboard
opt.clipboard = "unnamedplus"
if system == 'Darwin' then
    vim.g.clipboard = {
        name = 'macOS-clipboard',
        copy = {
            ['+'] = 'pbcopy',
            ['*'] = 'pbcopy'
        },
        paste = {
            ['+'] = 'pbpaste',
            ['*'] = 'pbpaste'
        },
        cache_enabled = 0
    }
end
