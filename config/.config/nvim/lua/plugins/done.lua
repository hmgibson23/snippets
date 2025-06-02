return {
  "monkoose/DoNe",
  config = function()
    -- as example adding some keybindings
    vim.keymap.set("n", "<F5>", "<Cmd>DoNe build<CR>")
    --- ...
  end,
}
