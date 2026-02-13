-- Lua 文件类型配置
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true

-- 运行快捷键
vim.keymap.set("n", "<leader>x", ":luafile %<CR>", { buffer = true, desc = "运行 Lua 文件" })
