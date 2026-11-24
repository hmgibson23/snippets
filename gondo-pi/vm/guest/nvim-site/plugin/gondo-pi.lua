-- gondo-pi (VM only; not part of your config): the system clipboard is your HOST's.
-- There is no display in the VM, so "+y / "*y go to your terminal as OSC 52 (zellij passes
-- it on), which puts the text on the host clipboard. Pasting uses nvim's own registers
-- (terminals don't answer OSC 52 reads by default).
if vim.g.gondo_pi_clipboard_done then return end
vim.g.gondo_pi_clipboard_done = true
local ok, osc52 = pcall(require, "vim.ui.clipboard.osc52")
if not ok then return end
local function paste()
  return { vim.fn.split(vim.fn.getreg(""), "\n"), vim.fn.getregtype("") }
end
vim.g.clipboard = {
  name = "gondo-pi OSC 52",
  copy = { ["+"] = osc52.copy("+"), ["*"] = osc52.copy("*") },
  paste = { ["+"] = paste, ["*"] = paste },
}
