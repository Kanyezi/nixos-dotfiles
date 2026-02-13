-- 基础选项设置
local opt = vim.opt

-- 编码
opt.encoding = "utf-8"
opt.fileencoding = "utf-8"

-- 外观
opt.number = true
opt.relativenumber = true
opt.cursorline = true
opt.signcolumn = "yes"
opt.showmode = false
opt.termguicolors = true
opt.background = "dark"

-- 缩进
opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.autoindent = true
opt.smartindent = true

-- 搜索
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- 编辑
opt.wrap = false
opt.scrolloff = 8
opt.sidescrolloff = 8
opt.splitbelow = true
opt.splitright = true
opt.timeoutlen = 300
opt.updatetime = 100
opt.clipboard = "unnamedplus"
opt.mouse = "a"

-- 备份
opt.backup = false
opt.writebackup = false
opt.swapfile = false
opt.undofile = true
opt.undodir = vim.fn.stdpath("data") .. "/undo"

-- 补全
opt.completeopt = "menu,menuone,noselect"
opt.wildmenu = true
opt.wildmode = "full"

