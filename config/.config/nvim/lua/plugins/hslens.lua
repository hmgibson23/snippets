return {
  "kevinhwang91/nvim-hlslens",
  event = "BufReadPre",
  keys = {
    -- Colemak: n/e/i are movement keys, so k/K keep Vim search-next/search-previous behavior.
    { "k", desc = "Search next with lens" },
    { "K", desc = "Search previous with lens" },
    { "<leader>hn", desc = "Search next with lens" },
    { "<leader>hN", desc = "Search previous with lens" },
    { "<leader>hc", desc = "Clear search highlight" },
    { "*", desc = "Search word forward" },
    { "#", desc = "Search word backward" },
    { "g*", desc = "Search word forward (partial)" },
    { "g#", desc = "Search word backward (partial)" },
  },
  config = function()
    require("hlslens").setup()

    local function map(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { noremap = true, silent = false, desc = desc })
    end

    map(
      "k",
      [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]],
      "Search next with lens"
    )
    map(
      "K",
      [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]],
      "Search previous with lens"
    )
    map(
      "<leader>hn",
      [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]],
      "Search next with lens"
    )
    map(
      "<leader>hN",
      [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]],
      "Search previous with lens"
    )
    map("*", [[*<Cmd>lua require('hlslens').start()<CR>]], "Search word forward")
    map("#", [[#<Cmd>lua require('hlslens').start()<CR>]], "Search word backward")
    map("g*", [[g*<Cmd>lua require('hlslens').start()<CR>]], "Search word forward (partial)")
    map("g#", [[g#<Cmd>lua require('hlslens').start()<CR>]], "Search word backward (partial)")
    map("<leader>hc", "<Cmd>noh<CR><Cmd>lua require('hlslens').stop()<CR>", "Clear search highlight")
  end,
}
