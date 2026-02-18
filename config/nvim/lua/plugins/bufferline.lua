-- Bufferline - 缓冲区标签栏
return {
  "akinsho/bufferline.nvim",
  event = "VeryLazy",
  dependencies = "nvim-tree/nvim-web-devicons",
  config = function()
    require("bufferline").setup({
      options = {
        mode = "buffers",
        numbers = "none",
        close_command = "bdelete! %d",
        right_mouse_command = "bdelete! %d",
        diagnostics = "nvim_lsp",
        show_buffer_close_icons = true,
        show_close_icon = true,
        separator_style = "slant",
        always_show_bufferline = false,
        offsets = {
          {
            filetype = "NvimTree",
            text = "File Explorer",
            highlight = "Directory",
            text_align = "left",
          },
        },
      },
    })
  end,
  keys = {
    { "<S-h>", "<cmd>BufferLineCyclePrev<cr>", desc = "上一个缓冲区" },
    { "<S-l>", "<cmd>BufferLineCycleNext<cr>", desc = "下一个缓冲区" },
    { "[b", "<cmd>BufferLineCyclePrev<cr>", desc = "上一个缓冲区" },
    { "]b", "<cmd>BufferLineCycleNext<cr>", desc = "下一个缓冲区" },
  },
}