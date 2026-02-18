-- ~/.config/nvim/lua/plugins/lsp.lua

return {
  -- Mason 用于安装 LSP 服务器
  {
    "williamboman/mason.nvim",
    lazy = false,
    config = true,
  },

  -- LSP 配置（Neovim 0.11+ 原生方式）
  {
    "neovim/nvim-lspconfig",
    event = "BufReadPre",
    config = function()
      -- Lua
      vim.lsp.config.lua_ls = {
        settings = {
          Lua = {
            diagnostics = { globals = { "vim" } },
            workspace = { library = vim.api.nvim_get_runtime_file("", true) },
            telemetry = { enable = false },
          },
        },
      }

      -- Python
      vim.lsp.config.pyright = {}

      -- C++
      vim.lsp.config.clangd = {}

      -- 启用 LSP 服务器
      vim.lsp.enable({ "lua_ls", "pyright", "clangd" })
    end,
  },
}