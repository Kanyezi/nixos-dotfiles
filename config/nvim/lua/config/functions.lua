-- 自定义函数
local M = {}

-- 切换背景透明
M.toggle_transparency = function()
  vim.g.transparent_enabled = not vim.g.transparent_enabled
  if vim.g.transparent_enabled then
    vim.cmd("highlight Normal guibg=NONE ctermbg=NONE")
    vim.cmd("highlight NonText guibg=NONE ctermbg=NONE")
  else
    vim.cmd("colorscheme " .. vim.g.colors_name)
  end
end

-- 快速运行当前文件
M.run_file = function()
  local ft = vim.bo.filetype
  local cmds = {
    python = "python3 %",
    lua = "lua %",
    sh = "bash %",
    cpp = "g++ % -o /tmp/a.out && /tmp/a.out",
    c = "gcc % -o /tmp/a.out && /tmp/a.out",
  }
  local cmd = cmds[ft]
  if cmd then
    vim.cmd("!" .. cmd)
  else
    print("Unsupported filetype: " .. ft)
  end
end

-- 注册命令
vim.api.nvim_create_user_command("ToggleTransparency", M.toggle_transparency, {})
vim.api.nvim_create_user_command("RunFile", M.run_file, {})

return M
