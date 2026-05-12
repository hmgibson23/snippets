vim.bo.expandtab = true
vim.bo.shiftwidth = 4
vim.bo.softtabstop = 4
vim.bo.tabstop = 4
vim.bo.commentstring = "// %s"
vim.opt_local.formatoptions:remove({ "o" })

local ok, csharp = pcall(require, "csharp")
if ok then
  vim.keymap.set("n", "<leader>cp", csharp.palette, { buffer = true, desc = "C# palette" })
  vim.keymap.set("n", "<leader>ci", csharp.info, { buffer = true, desc = "C# info" })
end
