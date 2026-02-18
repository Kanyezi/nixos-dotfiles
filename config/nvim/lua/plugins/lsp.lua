-- LSP 配置
return {
  -- Mason - LSP 服务器安装管理
  {
    "williamboman/mason.nvim",
    lazy = false,
    config = true,
  },

  -- 配置 LSP 服务器
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

      -- C/C++
      vim.lsp.config.clangd = {}

      -- 启用 LSP 服务器
      vim.lsp.enable("lua_ls", "pyright", "clangd")
    end,
  },
}