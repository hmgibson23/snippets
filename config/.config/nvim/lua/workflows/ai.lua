local select = require("workflows.select")

local M = {}

local function cmd(command)
  return function()
    vim.cmd(command)
  end
end

local function has_command(command)
  return vim.fn.exists(":" .. command) == 2
end

function M.actions()
  local actions = {}

  if has_command("CodeCompanionChat") then
    table.insert(actions, { label = "Chat: CodeCompanion", action = cmd("CodeCompanionChat") })
    table.insert(actions, { label = "Inline ask: CodeCompanion", action = cmd("CodeCompanion") })
    table.insert(actions, { label = "Actions: CodeCompanion", action = cmd("CodeCompanionActions") })
  end

  table.insert(actions, {
    label = "Agent: Pi toggle",
    action = function()
      require("pi").toggle()
    end,
  })
  table.insert(actions, {
    label = "Agent: Pi focus",
    action = function()
      require("pi").focus()
    end,
  })

  table.insert(actions, {
    label = "Agent: OpenCode toggle",
    action = function()
      require("opencode").toggle()
    end,
  })
  table.insert(actions, {
    label = "Agent: OpenCode ask cursor/selection",
    action = function()
      local mode = vim.fn.mode()
      if mode == "v" or mode == "V" or mode == "\22" then
        require("opencode").ask("@selection: ")
      else
        require("opencode").ask("@cursor: ")
      end
    end,
  })

  if has_command("AvanteAsk") then
    table.insert(actions, { label = "Workspace AI: Avante ask", action = cmd("AvanteAsk") })
  end
  if has_command("Aider") then
    table.insert(actions, { label = "Agent: Aider", action = cmd("Aider") })
  end
  if has_command("Copilot") then
    table.insert(actions, { label = "Inline: Copilot panel/status", action = cmd("Copilot status") })
  end

  return actions
end

function M.palette()
  select.run("AI actions", M.actions())
end

function M.setup()
  vim.api.nvim_create_user_command("AiPalette", M.palette, { desc = "Unified AI action palette" })
  vim.keymap.set({ "n", "v" }, "<leader>aP", M.palette, { desc = "AI palette" })
end

return M
