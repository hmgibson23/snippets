
return {
  "kevinhwang91/nvim-hlslens",
  event = "BufReadPre",
  config = function()
    require("hlslens").setup()

    local kopts = { noremap = true, silent = false }

    vim.api.nvim_set_keymap(
    "n",
    "k",
    [[<Cmd>execute('normal! ' . v:count1 . 'n')<CR><Cmd>lua require('hlslens').start()<CR>]],
    kopts
    )
    vim.api.nvim_set_keymap(
    "n",
    "K",
    [[<Cmd>execute('normal! ' . v:count1 . 'N')<CR><Cmd>lua require('hlslens').start()<CR>]],
    kopts
    )
    vim.api.nvim_set_keymap("n", "*", [[*<Cmd>lua require('hlslens').start()<CR>]], kopts)
    vim.api.nvim_set_keymap("n", "#", [[#<Cmd>lua require('hlslens').start()<CR>]], kopts)
    vim.api.nvim_set_keymap("n", "g*", [[g*<Cmd>lua require('hlslens').start()<CR>]], kopts)
    vim.api.nvim_set_keymap("n", "g#", [[g#<Cmd>lua require('hlslens').start()<CR>]], kopts)

    vim.api.nvim_set_keymap("n", "<Leader>u", "<Cmd>noh<CR>", kopts)
  end,
}
