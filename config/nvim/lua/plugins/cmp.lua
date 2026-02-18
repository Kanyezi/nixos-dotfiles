-- 代码补全配置
return {
  -- 核心补全引擎
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      -- LSP 补全源
      "hrsh7th/cmp-nvim-lsp",
      -- 缓冲区补全源
      "hrsh7th/cmp-buffer",
      -- 路径补全源
      "hrsh7th/cmp-path",
      -- 命令行补全源
      "hrsh7th/cmp-cmdline",
      -- 代码片段引擎
      {
        "L3MON4D3/LuaSnip",
        dependencies = "rafamadriz/friendly-snippets",
        build = "make install_jsregexp",
        config = function()
          require("luasnip").config.set_config({
            history = true,
            updateevents = "TextChanged,TextChangedI",
          })
        end,
      },
      -- 代码片段补全源
      "saadparwaiz1/cmp_luasnip",
      -- 图标支持
      "onsails/lspkind.nvim",
    },
    config = function()
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local lspkind = require("lspkind")

      -- 加载 friendly-snippets
      require("luasnip.loaders.from_vscode").lazy_load()

      -- 直接注册 C++ snippets
      local s = luasnip.snippet
      local t = luasnip.text_node
      local i = luasnip.insert_node

      luasnip.add_snippets("cpp", {
        s("xi", {
          t({"#include<bits/stdc++.h>", "using namespace std;", ""}),
          t({"#define int long long", "#define ll long long", "#define PII pair<int,int>", "#define Vi vector<int>", ""}),
          t({"int n,m,num,k,a,b,c,d,sum=0;", "const int NE=10000;", "string ss,s;", "char cc;", ""}),
          t({"", "void slove(){"}),
          t({"    "}), i(0),
          t({"}", "", "signed main(){"}),
          t({"    ios::sync_with_stdio(0);cin.tie(0);cout.tie(0);", "    // 不能使用printf,scanf", "    int t=1;", "    // cin >> t;", ""}),
          t({"    while (t--)", "    {", "        slove();", "    }", "    return 0;", "}" }),
        }),
      })

      cmp.setup({
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
              luasnip.expand_or_jump()
            else
              fallback()
            end
          end, { "i", "s" }),
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
              luasnip.jump(-1)
            else
              fallback()
            end
          end, { "i", "s" }),
        }),
        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "luasnip" },
        }, {
          { name = "buffer" },
          { name = "path" },
        }),
        formatting = {
          format = lspkind.cmp_format({
            mode = "symbol_text",
            maxwidth = 50,
            symbol_map = {
              Text = "󰉿",
              Method = "󰆧",
              Function = "󰊕",
              Constructor = "",
              Field = "󰜢",
              Variable = "󰀫",
              Class = "󰠱",
              Interface = "",
              Module = "",
              Property = "󰜢",
              Unit = "󰑭",
              Value = "󰎠",
              Enum = "",
              Keyword = "󰌋",
              Snippet = "",
              Color = "󰏘",
              File = "󰈙",
              Reference = "󰈇",
              Folder = "󰉋",
              EnumMember = "",
              Constant = "󰏿",
              Struct = "󰙅",
              Event = "",
              Operator = "󰆕",
              TypeParameter = "󰊄",
            },
          }),
        },
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
      })

      -- 命令行补全
      cmp.setup.cmdline("/", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = "buffer" },
        },
      })

      cmp.setup.cmdline(":", {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = "path" },
        }, {
          { name = "cmdline" },
        }),
      })
    end,
  },
}