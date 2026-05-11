-- OpenCode
return {
  "NickvanDyke/opencode.nvim",
  dependencies = {
    { "folke/snacks.nvim", opts = { input = { enabled = true } } },
  },
  keys = {
    {
      "<leader>aot",
      function()
        require("opencode").toggle()
      end,
      desc = "Toggle OpenCode",
    },
    {
      "<leader>aoA",
      function()
        require("opencode").ask()
      end,
      desc = "Ask OpenCode",
    },
    {
      "<leader>aoa",
      function()
        require("opencode").ask("@cursor: ")
      end,
      desc = "Ask OpenCode about this",
    },
    {
      "<leader>aoa",
      function()
        require("opencode").ask("@selection: ")
      end,
      desc = "Ask OpenCode about selection",
      mode = "v",
    },
    {
      "<leader>aon",
      function()
        require("opencode").command("session_new")
      end,
      desc = "New OpenCode session",
    },
    {
      "<leader>aoy",
      function()
        require("opencode").command("messages_copy")
      end,
      desc = "Copy last OpenCode response",
    },
    {
      "<S-C-u>",
      function()
        require("opencode").command("messages_half_page_up")
      end,
      desc = "Messages half page up",
    },
    {
      "<S-C-d>",
      function()
        require("opencode").command("messages_half_page_down")
      end,
      desc = "Messages half page down",
    },
    {
      "<leader>aos",
      function()
        require("opencode").select()
      end,
      desc = "Select OpenCode prompt",
      mode = { "n", "v" },
    },
    {
      "<leader>aoe",
      function()
        require("opencode").prompt("Explain @cursor and its context")
      end,
      desc = "Explain code (OpenCode)",
    },
  },
  init = function()
    vim.g.opencode_opts = {}
    vim.opt.autoread = true
  end,
}
