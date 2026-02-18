-- 主题配置
return {
  {
    "eldritch-theme/eldritch.nvim",
    lazy = false,
    priority = 1000,
    config = function()
        require("eldritch").setup({
            transparent = false
        })
        vim.cmd("colorscheme eldritch")
    end
  }, -- 其他主题（延迟加载）
  {
    "folke/tokyonight.nvim",
    name = "colors_tokyonight", -- 命名空间，避免冲突
    lazy = true,
    priority = 1000,
    config = function()
        require("tokyonight").setup({
            style = "moon"
        })
    end
  }
}
