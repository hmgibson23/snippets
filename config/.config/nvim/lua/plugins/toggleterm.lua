return {
  "akinsho/toggleterm.nvim",
  version = "*",
  cmd = { "ToggleTerm", "TermSelect", "TermExec" },
  keys = {
    { "<leader>T", group = "[T]erminal" },
    { "<leader>Tt", "<cmd>ToggleTerm<cr>", desc = "Toggle terminal" },
    { "<leader>Tl", "<cmd>TermSelect<cr>", desc = "Select terminal" },
    {
      "<leader>Tn",
      function()
        local current_dir = vim.fn.expand("%:p:h:t")
        local terminal = require("toggleterm.terminal").Terminal:new({
          cmd = "bash",
          hidden = true,
          direction = "float",
          name = current_dir ~= "" and current_dir or "terminal",
        })
        terminal:toggle()
      end,
      desc = "Named terminal",
    },
    {
      "<leader>Tp",
      function()
        require("toggleterm.terminal").Terminal:new({
          cmd = "python3",
          hidden = true,
          direction = "float",
          name = "python",
        }):toggle()
      end,
      desc = "Python REPL",
    },
    {
      "<leader>Tq",
      function()
        local function markdown_codeblock(language, content)
          return "\\`\\`\\`{" .. language .. "}\n" .. content .. "\n\\`\\`\\`"
        end

        local quarto_notebook_cmd = 'nvim -c enew -c "set filetype=quarto"'
          .. ' -c "norm GO## IPython\nThis is Quarto IPython notebook. Syntax is the same as in markdown\n\n'
          .. markdown_codeblock("python", "# enter code here\n")
          .. '"'
          .. ' -c "norm Gkk"'
          .. " -c \"lua require('lazy.core.loader').load({'molten-nvim', 'quarto-nvim'}, {cmd = 'Lazy load'})\""
          .. ' -c "MoltenInit python3" -c QuartoActivate -c startinsert'

        require("toggleterm.terminal").Terminal:new({
          cmd = quarto_notebook_cmd,
          hidden = true,
          direction = "float",
          name = "quarto-notebook",
        }):toggle()
      end,
      desc = "Quarto notebook terminal",
    },
  },
  opts = {
    hide_numbers = false,
    shade_terminals = true,
    direction = "vertical",
    size = 60,
    winbar = {
      enabled = false,
      name_formatter = function(term)
        return term.name
      end,
    },
  },
}
