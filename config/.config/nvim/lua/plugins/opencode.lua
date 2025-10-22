-- OpenCode
return {
  "NickvanDyke/opencode.nvim",
  dependencies = {
    { "folke/snacks.nvim", opts = { input = { enabled = true } } },
  },
  config = function()
    vim.g.opencode_opts = {}

    vim.opt.autoread = true

    vim.keymap.set("n", "<leader>aot", function()
      require("opencode").toggle()
    end, { desc = "Toggle OpenCode" })
    vim.keymap.set("n", "<leader>aoA", function()
      require("opencode").ask()
    end, { desc = "Ask OpenCode" })
    vim.keymap.set("n", "<leader>aoa", function()
      require("opencode").ask("@cursor: ")
    end, { desc = "Ask OpenCode about this" })
    vim.keymap.set("v", "<leader>aoa", function()
      require("opencode").ask("@selection: ")
    end, { desc = "Ask OpenCode about selection" })
    vim.keymap.set("n", "<leader>aon", function()
      require("opencode").command("session_new")
    end, { desc = "New OpenCode session" })
    vim.keymap.set("n", "<leader>aoy", function()
      require("opencode").command("messages_copy")
    end, { desc = "Copy last OpenCode response" })
    vim.keymap.set("n", "<S-C-u>", function()
      require("opencode").command("messages_half_page_up")
    end, { desc = "Messages half page up" })
    vim.keymap.set("n", "<S-C-d>", function()
      require("opencode").command("messages_half_page_down")
    end, { desc = "Messages half page down" })
    vim.keymap.set({ "n", "v" }, "<leader>aos", function()
      require("opencode").select()
    end, { desc = "Select OpenCode prompt" })

    vim.keymap.set("n", "<leader>aoe", function()
      require("opencode").prompt("Explain @cursor and its context")
    end, { desc = "Explain code (OpenCode)" })
  end,
}
