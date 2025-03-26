return {
  "TimUntersberger/neogit",
  config = function()
    require("neogit").setup({})
    local which_key = require("which-key")
    which_key.add({
      { "<leader>m",  group = "[M]Neogit" },
      { "<leader>mm", ":Neogit<CR>",      desc = "Neogit" },
    })
  end,
}
