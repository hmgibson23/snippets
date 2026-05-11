local M = {}

local modules = {
  "workflows.project",
  "workflows.ai",
  "workflows.code",
  "workflows.tasks",
  "workflows.diagnostics",
  "workflows.health",
  "workflows.sessions",
}

function M.setup()
  for _, module in ipairs(modules) do
    local ok, loaded = pcall(require, module)
    if ok and type(loaded.setup) == "function" then
      loaded.setup()
    elseif not ok then
      vim.notify("Failed to load " .. module .. ": " .. loaded, vim.log.levels.ERROR)
    end
  end

  vim.api.nvim_create_user_command("WorkspacePalette", function()
    require("workflows.palette").open()
  end, { desc = "Workspace command palette" })

  vim.keymap.set("n", "<leader><leader>", function()
    require("workflows.palette").open()
  end, { desc = "Workspace palette" })
end

return M
