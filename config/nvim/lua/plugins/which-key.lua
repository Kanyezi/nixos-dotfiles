-- WhichKey - 快捷键提示
return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  config = function()
    local wk = require("which-key")
    wk.setup({
      icons = {
        group = "",
      },
    })

    -- 注册 leader 快捷键
    wk.add({
      { "<leader>", group = "leader" },
      { "<leader>b", group = "buffer" },
      { "<leader>e", desc = "切换文件树" },
      { "<leader>f", group = "format" },
      { "<leader>g", group = "git" },
      { "<leader>l", group = "lsp" },
      { "<leader>q", group = "quit" },
      { "<leader>s", group = "search" },
      { "<leader>t", group = "terminal" },
      { "<leader>w", group = "write" },
    })
  end,
}