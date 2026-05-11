local M = {}

function M.foldexpr()
  if not vim.treesitter or not vim.treesitter.foldexpr then
    return "0"
  end

  local ok, result = pcall(vim.treesitter.foldexpr)
  if not ok or result == nil then
    return "0"
  end

  return result
end

local function apply()
  if vim.bo.buftype ~= "" then
    return
  end

  vim.opt_local.foldmethod = "expr"
  vim.opt_local.foldexpr = "v:lua.require'utils.folds'.foldexpr()"
  vim.opt_local.foldlevel = 99
  vim.opt_local.foldlevelstart = 99
  vim.opt_local.foldenable = true
end

function M.setup()
  vim.opt.foldlevel = 99
  vim.opt.foldlevelstart = 99
  vim.opt.foldenable = true
  apply()

  vim.api.nvim_create_autocmd({ "BufWinEnter", "FileType" }, {
    group = vim.api.nvim_create_augroup("SafeTreesitterFolds", { clear = true }),
    callback = apply,
  })
end

return M
