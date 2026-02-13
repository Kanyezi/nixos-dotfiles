-- Markdown 文件类型配置
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.spell = false
vim.opt_local.conceallevel = 2

-- 快捷键
vim.keymap.set("n", "<leader>mp", ":MarkdownPreview<CR>", { buffer = true, desc = "Markdown 预览" })
