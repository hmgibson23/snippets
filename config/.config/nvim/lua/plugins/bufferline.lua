return {
  "akinsho/bufferline.nvim",
  version = "*",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    options = {
      toggle_hidden_on_enter = true,
      diagnostics_indicator = function(count, level, diagnostics_dict, context)
        local s = " "
        for e, n in pairs(diagnostics_dict) do
          local sym = e == "error" and " " or (e == "warning" and " " or " ")
          s = s .. n .. sym
        end
        return s
      end,
      groups = {
        items = {
          {
            name = "Tests", -- Mandatory
            highlight = { underline = true, sp = "blue" }, -- Optional
            priority = 2, -- Determines the group's display priority
            icon = " ", -- Optional
            matcher = function(buf) -- Mandatory
              return buf.filename:match("_test") or buf.filename:match("_spec")
            end,
          },
          {
            name = "Quarto", -- Mandatory
            highlight = { underline = true, sp = "blue" }, -- Optional
            priority = 2, -- Determines the group's display priority
            icon = " ", -- Optional
            matcher = function(buf) -- Mandatory
              return buf.filename:match("%.qmd")
            end,
          },
          {
            name = "Docs",
            highlight = { undercurl = true, sp = "green" },
            auto_close = false, -- Keep the group even if it doesn't contain the current buffer
            matcher = function(buf)
              return buf.filename:match("%.md") or buf.filename:match("%.txt")
            end,
            separator = { -- Optional separator configuration
              style = require("bufferline.groups").separator.tab,
            },
          },
        },
      },
    },
  },
}
