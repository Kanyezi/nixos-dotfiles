-- LSP 配置插件
return {
  -- LSP 配置
  {
    "neovim/nvim-lspconfig",
    event = "BufReadPre",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      -- 使用 Neovim 0.11+ 新 API 配置 LSP
      -- Lua
      vim.lsp.config.lua_ls = {
        capabilities = capabilities,
        settings = {
          Lua = {
            diagnostics = { globals = { "vim" } },
            workspace = { library = vim.api.nvim_get_runtime_file("", true) },
            telemetry = { enable = false },
          },
        },
      }

      -- Python
      vim.lsp.config.pyright = {
        capabilities = capabilities,
      }

      -- C/C++
      vim.lsp.config.clangd = {
        capabilities = capabilities,
      }

      -- 自动启用 LSP 服务器
      vim.lsp.enable("lua_ls")
      vim.lsp.enable("pyright")
      vim.lsp.enable("clangd")
    end,
  },

  -- LSP 安装管理
  {
    "williamboman/mason.nvim",
    build = ":MasonUpdate",
    config = function()
      require("mason").setup()
    end,
  },
  {
    "williamboman/mason-lspconfig.nvim",
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = { "lua_ls", "pyright", "clangd" },
        automatic_installation = true,
      })
    end,
  },
}