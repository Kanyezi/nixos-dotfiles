-- 快捷键映射
local keymap = vim.keymap
local opts = { noremap = true, silent = true }

-- Leader 键
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- 窗口操作
keymap.set("n", "<leader>sv", "<C-w>v", { desc = "垂直分割" })
keymap.set("n", "<leader>sh", "<C-w>s", { desc = "水平分割" })
keymap.set("n", "<leader>sc", "<C-w>c", { desc = "关闭窗口" })
keymap.set("n", "<leader>so", "<C-w>o", { desc = "关闭其他窗口" })
keymap.set("n", "<C-h>", "<C-w>h", opts)
keymap.set("n", "<C-j>", "<C-w>j", opts)
keymap.set("n", "<C-k>", "<C-w>k", opts)
keymap.set("n", "<C-l>", "<C-w>l", opts)

-- 缓冲区操作
keymap.set("n", "<leader>bn", ":bnext<CR>", { desc = "下一个缓冲区" })
keymap.set("n", "<leader>bp", ":bprevious<CR>", { desc = "上一个缓冲区" })
keymap.set("n", "<leader>bd", ":bdelete<CR>", { desc = "关闭缓冲区" })

-- 文件操作
keymap.set("n", "<leader>w", ":write<CR>", { desc = "保存文件" })
keymap.set("n", "<leader>q", ":quit<CR>", { desc = "退出" })
keymap.set("n", "<leader>Q", ":qa!<CR>", { desc = "强制退出" })

-- 编辑操作
keymap.set("i", "jj", "<Esc>", opts)
keymap.set("n", "<leader>u", ":undo<CR>", { desc = "撤销" })
keymap.set("n", "<leader>r", ":redo<CR>", { desc = "重做" })

-- 移动操作
keymap.set("n", "H", "^", opts)
keymap.set("n", "L", "$", opts)
keymap.set("v", "H", "^", opts)
keymap.set("v", "L", "$", opts)

-- 搜索高亮
keymap.set("n", "<Esc>", ":noh<CR>", opts)

-- 终端
keymap.set("n", "<leader>t", ":terminal<CR>", { desc = "打开终端" })
keymap.set("t", "<Esc>", "<C-\\><C-n>", opts)

-- 文件树快捷键已在 nvim-tree.lua 中配置

-- LSP 快捷键（LspAttach 时生效）
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("LspKeymaps", {}),
  callback = function(ev)
    local bufopts = { buffer = ev.buf, silent = true }
    keymap.set("n", "gD", vim.lsp.buf.declaration, vim.tbl_extend("force", bufopts, { desc = "跳转到声明" }))
    keymap.set("n", "gd", vim.lsp.buf.definition, vim.tbl_extend("force", bufopts, { desc = "跳转到定义" }))
    keymap.set("n", "K", vim.lsp.buf.hover, vim.tbl_extend("force", bufopts, { desc = "显示文档" }))
    keymap.set("n", "gi", vim.lsp.buf.implementation, vim.tbl_extend("force", bufopts, { desc = "跳转到实现" }))
    keymap.set("n", "gr", vim.lsp.buf.references, vim.tbl_extend("force", bufopts, { desc = "查找引用" }))
    keymap.set("n", "<leader>rn", vim.lsp.buf.rename, vim.tbl_extend("force", bufopts, { desc = "重命名" }))
    keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, vim.tbl_extend("force", bufopts, { desc = "代码操作" }))
    keymap.set("n", "<leader>f", function()
      vim.lsp.buf.format({ async = true })
    end, vim.tbl_extend("force", bufopts, { desc = "格式化" }))
  end,
})
